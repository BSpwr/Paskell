-- HSpec tests for Interpreter.hs
module Main where

import           Test.Hspec
import           Test.QuickCheck
import           Control.Exception              ( evaluate )
import           Interpreter
import           TypeAST
import           Control.Monad.State
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , toLower
                                                )

main :: IO ()
main = hspec $ do
    describe "Expressions" $ do
        it "adds integers" $ do
            s <- evalStateT
                (evalExpr (Sum (VExpr $ Int 5) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VInt 9)
        it "adds floats and integers" $ do
            s <- evalStateT
                (evalExpr (Sum (VExpr $ Double 5.0) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 9.0)
        it "adds floats" $ do
            s <- evalStateT
                (evalExpr (Sum (VExpr $ Double 5.2) (VExpr $ Double 10.0)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 15.2)

        it "subtracts integers" $ do
            s <- evalStateT
                (evalExpr (Sub (VExpr $ Int 5) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VInt 1)
        it "subtracts floats and integers" $ do
            s <- evalStateT
                (evalExpr (Sub (VExpr $ Double 5.0) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 1.0)
        it "subtracts floats" $ do
            s <- evalStateT
                (evalExpr (Sub (VExpr $ Double 5.2) (VExpr $ Double 10.0)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble (-4.8))

        it "multiplies integers" $ do
            s <- evalStateT
                (evalExpr (Mul (VExpr $ Int 5) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VInt 20)
        it "multiplies floats and integers" $ do
            s <- evalStateT
                (evalExpr (Mul (VExpr $ Double 5.0) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 20.0)
        it "multiplies floats" $ do
            s <- evalStateT
                (evalExpr (Mul (VExpr $ Double 5.2) (VExpr $ Double 10.0)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 52.0)

        it "divides integers" $ do
            s <- evalStateT
                (evalExpr (Div (VExpr $ Int 5) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VInt 1)
        it "divides floats and integers" $ do
            s <- evalStateT
                (evalExpr (Div (VExpr $ Double 5.0) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 1.25)
        it "divides floats" $ do
            s <- evalStateT
                (evalExpr (Div (VExpr $ Double 5.2) (VExpr $ Double 10.0)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 0.52)

        it "does modulo on integers" $ do
            s <- evalStateT
                (evalExpr (Mod (VExpr $ Int 5) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VInt 1)
        it "does modulo on floats and integers" $ do
            s <- evalStateT
                (evalExpr (Mod (VExpr $ Double 5.0) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 1.00)
        it "does modulo on floats" $ do
            s <- evalStateT
                (evalExpr (Mod (VExpr $ Double 52.5) (VExpr $ Double 10.0)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VDouble 2.5)

        it "does equality check (false case)" $ do
            s <- evalStateT
                (evalExpr (Eq (VExpr $ Double 5.1) (VExpr $ Int 5)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool False)
        it "does equality check (true case)" $ do
            s <- evalStateT
                (evalExpr (Eq (VExpr $ Int 4) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool True)

        it "does not equal check (true case)" $ do
            s <- evalStateT
                (evalExpr (NotEq (VExpr $ Double 5.1) (VExpr $ Int 5)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool True)
        it "does not equal check (false case)" $ do
            s <- evalStateT
                (evalExpr (NotEq (VExpr $ Int 4) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool False)

        it "does greater than check (true case)" $ do
            s <- evalStateT
                (evalExpr (GreaterThan (VExpr $ Double 5.1) (VExpr $ Int 5)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool True)
        it "does greater than check (false case)" $ do
            s <- evalStateT
                (evalExpr (GreaterThan (VExpr $ Int 4) (VExpr $ Int 6)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool False)

        it "does less than check (false case)" $ do
            s <- evalStateT
                (evalExpr (LessThan (VExpr $ Double 5.1) (VExpr $ Int 5)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool False)
        it "does less than check (true case)" $ do
            s <- evalStateT
                (evalExpr (LessThan (VExpr $ Double 1.9) (VExpr $ Int 4)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool True)

        it "not boolean" $ do
            s <- evalStateT
                (evalExpr (Not (VExpr BFalse)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VBool True)
        it "negate integer bits" $ do
            s <- evalStateT
                (evalExpr (Not (VExpr $ Int 999)))
                (NormalStatus, (Map.empty, Map.empty, Map.empty, Map.empty))
            s `shouldBe` (VInt (-1000))

    describe "Assignment" $ do
        it "assigns values when types match" $ do
            (iStatus, (constTable, varTable, typeTable, funcTable)) <-
                execStateT
                    (execStatement
                        (Assign
                            ( pack "VarName"
                            , Sum (VExpr $ Int 5) (VExpr $ Int 4)
                            )
                        )
                    )
                    ( NormalStatus
                    , ( Map.empty
                      , Map.fromList [(pack "VarName", (IntType, Nothing))]
                      , Map.empty
                      , Map.empty
                      )
                    )
            varTable
                `shouldBe` (Map.fromList
                               [(pack "VarName", (IntType, Just (VInt 9)))]
                           )
        it "assigns values when types match 2" $ do
            (iStatus, (constTable, varTable, typeTable, funcTable)) <-
                execStateT
                    (execStatement
                        (Assign
                            ( pack "VarName"
                            , Sum (VExpr $ Double 5.0) (VExpr $ Double 4.0)
                            )
                        )
                    )
                    ( NormalStatus
                    , ( Map.empty
                      , Map.fromList [(pack "VarName", (RealType, Nothing))]
                      , Map.empty
                      , Map.empty
                      )
                    )
            varTable
                `shouldBe` (Map.fromList
                               [ ( pack "VarName"
                                 , (RealType, Just (VDouble 9.0))
                                 )
                               ]
                           )

    describe "If Statement" $ do
        it "branches correctly on True" $ do
            (iStatus, (constTable, varTable, typeTable, funcTable)) <-
                execStateT
                    (execStatement
                        (StatementIf
                            (VExpr BTrue)
                            (Assign
                                ( pack "VarName"
                                , Sum (VExpr $ Double 5.0) (VExpr $ Double 4.0)
                                )
                            )
                            ( Just
                            $ Assign
                                  ( pack "VarName"
                                  , Sum (VExpr $ Double 4.0)
                                        (VExpr $ Double 1.0)
                                  )
                            )
                        )
                    )
                    ( NormalStatus
                    , ( Map.empty
                      , Map.fromList [(pack "VarName", (RealType, Nothing))]
                      , Map.empty
                      , Map.empty
                      )
                    )
            varTable
                `shouldBe` (Map.fromList
                               [ ( pack "VarName"
                                 , (RealType, Just (VDouble 9.0))
                                 )
                               ]
                           )
        it "branches correctly on False" $ do
            (iStatus, (constTable, varTable, typeTable, funcTable)) <-
                execStateT
                    (execStatement
                        (StatementIf
                            (VExpr BFalse)
                            (Assign
                                ( pack "VarName"
                                , Sum (VExpr $ Double 5.0) (VExpr $ Double 4.0)
                                )
                            )
                            ( Just
                            $ Assign
                                  ( pack "VarName"
                                  , Sum (VExpr $ Double 4.0)
                                        (VExpr $ Double 1.0)
                                  )
                            )
                        )
                    )
                    ( NormalStatus
                    , ( Map.empty
                      , Map.fromList [(pack "VarName", (RealType, Nothing))]
                      , Map.empty
                      , Map.empty
                      )
                    )
            varTable
                `shouldBe` (Map.fromList
                               [ ( pack "VarName"
                                 , (RealType, Just (VDouble 5.0))
                                 )
                               ]
                           )

    describe "Code block" $ do
        it "executes sequentially" $ do
            (iStatus, (constTable, varTable, typeTable, funcTable)) <-
                execStateT
                    (execStatement
                        (StatementBlock
                            [ (Assign
                                  ( pack "Var1"
                                  , Sum (VExpr $ Double 4.0)
                                        (VExpr $ Double 52.2)
                                  )
                              )
                            , (StatementIf
                                  (VExpr BTrue)
                                  (Assign
                                      ( pack "VarName"
                                      , Sum (VarCall (pack "Var1") [])
                                            (VExpr $ Double 4.0)
                                      )
                                  )
                                  ( Just
                                  $ Assign
                                        ( pack "VarName"
                                        , Sum (VExpr $ Double 4.0)
                                              (VExpr $ Double 1.0)
                                        )
                                  )
                              )
                            ]
                        )
                    )
                    ( NormalStatus
                    , ( Map.empty
                      , Map.fromList
                          [ (pack "Var1"   , (RealType, Nothing))
                          , (pack "VarName", (RealType, Nothing))
                          ]
                      , Map.empty
                      , Map.empty
                      )
                    )
            varTable
                `shouldBe` (Map.fromList
                               [ (pack "Var1", (RealType, Just (VDouble 56.2)))
                               , ( pack "VarName"
                                 , (RealType, Just (VDouble 60.2))
                                 )
                               ]
                           )
