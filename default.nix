{ mkDerivation, base, bifunctors, bytestring, concurrent-supply
, containers, contravariant, data-default, dependent-map
, dependent-sum, dependent-sum-template, dlist, ghcjs-dom, lens
, mtl, reducers, reflex, reflex-transformers, semigroups
, stateWriter, stdenv, text, transformers
}:
mkDerivation {
  pname = "reflex-html";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bifunctors bytestring concurrent-supply containers
    contravariant data-default dependent-map dependent-sum
    dependent-sum-template dlist ghcjs-dom lens mtl reducers reflex
    reflex-transformers semigroups stateWriter text transformers
  ];
  executableHaskellDepends = [
    base containers data-default lens mtl reflex reflex-transformers
    transformers
  ];
  description = "HTML DSL for reflex-dom";
  license = stdenv.lib.licenses.bsd3;
}
