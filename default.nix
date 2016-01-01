{ mkDerivation, aeson, attoparsec, base, bytestring
, case-insensitive, deepseq, directory, filepath, Glob, http-kit
, http-types, network, network-uri, parsec, process, stdenv, text
, transformers, unix, utf8-string, vector
}:
mkDerivation {
  pname = "fsrest";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring case-insensitive deepseq directory
    filepath Glob http-kit http-types network network-uri parsec
    process text transformers unix utf8-string vector
  ];
  homepage = "https://github.com/jekor/fsrest";
  description = "filesystem-based RESTful HTTP server";
  license = stdenv.lib.licenses.mit;
}
