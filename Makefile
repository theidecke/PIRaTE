build :
	ghc -O2 -fexcess-precision -funfolding-use-threshold=48 -threaded --make Main.hs -o Main

cleanbuild :
	ghc -O2 -fexcess-precision -funfolding-use-threshold=48 -threaded -fforce-recomp --make Main.hs -o Main

buildprofilable : PIRaTE
  ghc -O2 -fexcess-precision -funfolding-use-threshold=48 --make Main.hs -prof -auto-all -caf-all -fforce-recomp -o Main
