{ mkDerivation, aeson, base, bytestring, errors, http-conduit
, http-types, lib, mtl, open-browser, protolude, servant
, servant-server, text, transformers, unordered-containers, warp
}:
mkDerivation {
  pname = "als";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring errors http-conduit http-types mtl
    open-browser protolude servant servant-server text transformers
    unordered-containers warp
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/githubuser/als#readme";
  license = lib.licenses.bsd3;
}
