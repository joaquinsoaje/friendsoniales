module Spec where

import PdePreludat
import Library
import Test.Hspec

cecilia = Persona { nombre = "Cecilia", edad = 30, alegria = 90, ansiedad = 40, cantidadTareas = 0 }
juan = Persona { nombre = "Juan", edad = 30, alegria = 80, ansiedad = 250, cantidadTareas = 0 }

correrTests :: IO ()
correrTests = hspec $ do
  describe "cuantoDueleVerLasBuenas" $ do
    it "Jovenes con poca energia" $ do
      cuantoDueleVerLasBuenas [cecilia, juan] `shouldBe` False
    it "Jovenes con mucha energia" $ do
      let ceciliaEnergica = cecilia { ansiedad = 0 }
      let juanEnergico = juan { ansiedad = 0 }
      cuantoDueleVerLasBuenas [ceciliaEnergica, juanEnergico] `shouldBe` True

  describe "nivelTotalDeAnsiedad" $ do
    it "Juan y Cecilia" $ do
      nivelTotalDeAnsiedad [cecilia, juan] `shouldBe` 290
  describe "losMasCriticados" $ do
    it "Nivel de ansiedad > 50" $ do
      losMasCriticados ((>50).ansiedad) [cecilia, juan] `shouldBe` ["Juan"]
    it "Energia par" $ do
      losMasCriticados (even.nivelEnergia) [cecilia, juan] `shouldBe` ["Cecilia", "Juan"]

  describe "tareas" $ do
    describe "hacerTramitesEnAfip" $ do
      it "Cecilia hace 2 trámites en AFIP" $ do
        let personaFinal = (hacerTramitesEnAfip 2) cecilia
        ansiedad personaFinal `shouldBe` 290
      it "Juan hace 2 trámites en AFIP" $ do
        let personaFinal = (hacerTramitesEnAfip 2) juan
        ansiedad personaFinal `shouldBe` 490

    describe "andarEnBici" $ do
      it "Cecilia anda en bici" $ do
        let ceciliaEnBici = cecilia { ansiedad = 65 }
        let personaFinal = (andarEnBici 1) ceciliaEnBici
        ansiedad personaFinal `shouldBe` 0
        alegria personaFinal `shouldBe` 140

    describe "escucharMusica" $ do
      it "santiago escucharMusica" $ do
        let santiago = cecilia { ansiedad = 30 }
        let personaFinal = escucharMusica santiago
        ansiedad personaFinal `shouldBe` 10

    describe "codearUnProyectoNuevo" $ do
      it "Juan codearUnProyectoNuevo" $ do
        let juan = cecilia { alegria = 100, ansiedad = 100 }
        let personaFinal = codearUnProyectoNuevo juan
        ansiedad personaFinal `shouldBe` 140
        alegria personaFinal `shouldBe` 210

  describe "Pruebas de hiceLoQuePude" $ do
    it "Debe devolver la persona con menos tareas si tiene suficiente energía" $ do
      let persona = Persona { nombre = "persona", edad = 30, alegria = 80, ansiedad = 50, cantidadTareas = 3 }
      let tareas = [andarEnBici 10, codearUnProyectoNuevo, escucharMusica]
      let personaFinal = hiceLoQuePude persona tareas
      cantidadTareas personaFinal `shouldBe` 0

    it "Debe devolver la misma persona si no tiene suficiente energía para hacer ninguna tarea" $ do
      let persona = Persona { nombre = "persona", edad = 25, alegria = 30, ansiedad = 80, cantidadTareas = 2 }
      let tareas = [hacerTramitesEnAfip 5, escucharMusica]
      let personaFinal = hiceLoQuePude persona tareas
      personaFinal `shouldBe` persona

    it "Debe devolver la persona con menos tareas si tiene suficiente energía para algunas tareas" $ do
      let persona = Persona { nombre = "persona", edad = 35, alegria = 100, ansiedad = 20, cantidadTareas = 5 }
      let tareas = [codearUnProyectoNuevo, escucharMusica, hacerTramitesEnAfip 50, andarEnBici 5]
      let personaFinal = hiceLoQuePude persona tareas
      cantidadTareas personaFinal `shouldBe` 1

  describe "Pruebas de energiaResultante" $ do
    it "Debe devolver el nivel de energía correcto después de realizar todas las tareas" $ do
      let persona = Persona { nombre = "persona", edad = 28, alegria = 50, ansiedad = 30, cantidadTareas = 3 }
      let tareas = [codearUnProyectoNuevo, hacerTramitesEnAfip 2, andarEnBici 7]
      let personaFinal = energiaResultante persona tareas
      personaFinal `shouldBe` 340

    it "Debe devolver el nivel de energía correcto si no se realizan tareas" $ do
      let persona = Persona { nombre = "persona", edad = 40, alegria = 70, ansiedad = 40, cantidadTareas = 0 }
      let tareas = []
      let personaFinal = energiaResultante persona tareas
      personaFinal `shouldBe` 140
