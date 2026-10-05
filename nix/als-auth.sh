if [[ $EUID -ne 0 ]]; then
  echo 'Run sudo als-auth to authenticate the ALS service.' >&2
  exit 1
fi
if [[ ! -t 0 || ! -t 1 ]]; then
  echo 'als-auth needs a terminal. Over SSH, use ssh -t HOST sudo als-auth.' >&2
  exit 1
fi
exec 9>/run/lock/als-auth.lock
if ! flock -n 9; then
  echo 'Another ALS sign-in is already running.' >&2
  exit 1
fi

echo 'Stopping ALS while you sign in…'
systemctl stop als.service
# The transient unit creates/owns the same state directory even before the
# worker has ever started. Conflicts + ordering prevent concurrent token writes.
als_auth_unit="als-auth-$(systemd-id128 new)"
if systemd-run --quiet --wait --collect --pty --unit="$als_auth_unit" \
    --property=User=als --property=Group=als \
    --property=StateDirectory=als --property=StateDirectoryMode=0700 \
    --property=Conflicts=als.service --property=After=als.service \
    --property=UMask=0077 --property=NoNewPrivileges=yes \
    --property=ProtectSystem=strict --property=ProtectHome=yes \
    --property=PrivateTmp=yes \
    "${als_auth_environment[@]}" \
    "$als_auth_package/bin/als" --auth; then
  systemctl reset-failed als.service
  systemctl start als.service
  echo 'Sign-in saved. ALS has been started.'
else
  echo 'Sign-in did not complete. ALS remains stopped. Run sudo als-auth to try again.' >&2
  exit 1
fi
