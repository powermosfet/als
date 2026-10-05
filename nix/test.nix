{ pkgs, module }:
let
  # A deterministic login backend lets the VM verify service ownership,
  # orchestration, cancellation and restart policy without live credentials.
  fakeAls = pkgs.writeShellApplication {
    name = "als";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      if [[ "''${1:-}" == --auth ]]; then
        trap 'exit 1' INT TERM
        id -un > /var/lib/als/login-user
        echo 'Open https://example.invalid and enter ABCD-EFGH'
        touch /var/lib/als/login-started
        if [[ -e /var/lib/als/decline ]]; then exit 1; fi
        if [[ -e /var/lib/als/hold ]]; then sleep 60; fi
        printf '{"access_token":"test-access","refresh_token":"test-refresh"}\n' > "$TOKEN_FILE.new"
        chmod 600 "$TOKEN_FILE.new"
        mv "$TOKEN_FILE.new" "$TOKEN_FILE"
        rm -f /var/lib/als/expired
        exit 0
      fi
      if [[ ! -e "$TOKEN_FILE" || -e /var/lib/als/expired ]]; then
        echo 'authentication-required'
        exit 78
      fi
      exec sleep infinity
    '';
  };
in pkgs.testers.runNixOSTest {
  name = "als-nixos-auth";
  globalTimeout = 180;
  nodes.machine = { ... }: {
    imports = [ module ];
    services.als = {
      enable = true;
      package = fakeAls;
      clientId = "test-client";
      listId = "test-list";
    };
    environment.systemPackages = [ pkgs.util-linux ];
  };
  testScript = ''
    from datetime import timedelta

    start_all()
    machine.wait_for_unit("multi-user.target")
    machine.wait_until_succeeds("test $(systemctl show als -p ExecMainStatus --value) = 78")
    machine.succeed("test $(systemctl show als -p NRestarts --value) = 0")
    machine.succeed("test $(stat -c %a /var/lib/als) = 700")

    with subtest("first login needs only one command"):
        machine.succeed("script -q -e -c als-auth /dev/null")
        machine.wait_for_unit("als.service")
        machine.succeed("test $(cat /var/lib/als/login-user) = als")
        machine.succeed("test $(stat -c %U /var/lib/als/tokens.json) = als")
        machine.succeed("test $(stat -c %a /var/lib/als/tokens.json) = 600")
        machine.succeed("test $(systemctl show als -p Environment --value | grep -o CLIENT_ID=test-client) = CLIENT_ID=test-client")

    with subtest("declined login preserves tokens and leaves worker stopped"):
        machine.succeed("cp /var/lib/als/tokens.json /tmp/original-tokens; touch /var/lib/als/decline")
        machine.fail("script -q -e -c als-auth /dev/null > /tmp/declined-login.log 2>&1")
        machine.succeed("grep -q ABCD-EFGH /tmp/declined-login.log")
        machine.fail("systemctl is-active --quiet als")
        machine.succeed("cmp /tmp/original-tokens /var/lib/als/tokens.json; rm /var/lib/als/decline")

    with subtest("exclusive login and cancellation"):
        machine.succeed("rm /var/lib/als/login-started; touch /var/lib/als/hold")
        machine.succeed("systemd-run --unit=als-helper-test --collect /run/current-system/sw/bin/bash -c '/run/current-system/sw/bin/script -q -e -c /run/current-system/sw/bin/als-auth /dev/null > /tmp/login.log 2>&1'")
        machine.wait_for_file("/var/lib/als/login-started", timeout=timedelta(seconds=30))
        machine.fail("script -q -e -c als-auth /dev/null > /tmp/concurrent-login.log 2>&1")
        machine.succeed("grep -q 'Another ALS sign-in' /tmp/concurrent-login.log")
        machine.fail("systemctl is-active --quiet als")
        machine.succeed("systemctl stop 'als-auth-*.service'")
        machine.wait_until_succeeds("! systemctl is-active --quiet als-helper-test", timeout=timedelta(seconds=30))
        machine.succeed("grep -q 'ALS remains stopped' /tmp/login.log || { cat /tmp/login.log; exit 1; }")
        machine.fail("systemctl is-active --quiet als")
        machine.succeed("cmp /tmp/original-tokens /var/lib/als/tokens.json; rm /var/lib/als/hold")
        machine.succeed("script -q -e -c als-auth /dev/null")
        machine.wait_for_unit("als.service")

    with subtest("revocation stops restart loop and login recovers"):
        machine.succeed("touch /var/lib/als/expired; systemctl restart als")
        machine.wait_until_succeeds("test $(systemctl show als -p ExecMainStatus --value) = 78")
        machine.succeed("sleep 6; test $(systemctl show als -p NRestarts --value) = 0")
        machine.succeed("script -q -e -c als-auth /dev/null")
        machine.wait_for_unit("als.service")
  '';
}
