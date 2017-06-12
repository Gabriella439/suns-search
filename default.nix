{ mkDerivation, aeson, amqp, array, attoparsec, base, bytestring
, Cabal, containers, deepseq, directory, errors, filepath
, haskeline, hmatrix, hslogger, mmorph, MonadRandom
, optparse-applicative, pipes, pipes-concurrency, stdenv, text
, text-format, transformers, unordered-containers, vector
}:
mkDerivation {
  pname = "suns-search";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson amqp array attoparsec base bytestring Cabal containers
    deepseq directory errors filepath haskeline hmatrix hslogger mmorph
    MonadRandom optparse-applicative pipes pipes-concurrency text
    text-format transformers unordered-containers vector
  ];
  license = stdenv.lib.licenses.gpl2;
}
