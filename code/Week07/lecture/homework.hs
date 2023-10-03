{-# LANGUAGE OverloadedStrings #-}
module Homework where

import Language.Marlowe.Extended.V1 hiding (contract)

main :: IO ()
main = printJSON $ contract "Alice" "Bob" "Charlie" $ Constant 10000000

choice :: Party -> ChoiceId
choice = ChoiceId "winner"

fstDepositTimeout, sndDepositTimeout, thirdDepositTimeout, makingChoiceTimeout :: Timeout
fstDepositTimeout   = 1695609360000
sndDepositTimeout   = 1695612960000
thirdDepositTimeout = 1695616560000
makingChoiceTimeout = 1695577080000


{- Define a contract, Close is the simplest contract which just ends the contract straight away
-}
contract :: Party -> Party -> Party -> Value -> Contract
contract alice bob charlie deposit = 
    When
        [Case
            (Deposit
                charlie
                charlie
                ada
                (AddValue
                    deposit
                    deposit
                )
            )
            (When
                [ f alice bob
                , f bob   alice
                ]
                sndDepositTimeout Close 
            )]
        fstDepositTimeout Close 
    where
        f :: Party -> Party -> Case
        f pA pB = 
            Case
                (Deposit
                    pA
                    pA
                    ada
                    deposit
                )
                (When
                    [Case
                        (Deposit
                            pB
                            pB
                            ada
                            deposit
                        )
                        (When
                            [Case
                                (Choice
                                    (choice charlie)
                                    [Bound 1 2]
                                )
                                (If
                                    (ValueEQ
                                        (ChoiceValue $ choice charlie)
                                        (Constant 1)
                                    )
                                    (Pay
                                        bob
                                        (Account alice)
                                        ada
                                        deposit
                                        Close 
                                    )
                                    (Pay
                                        alice
                                        (Account bob)
                                        ada
                                        deposit
                                        Close 
                                    )
                                )]
                            makingChoiceTimeout
                            (Pay
                                charlie
                                (Account alice)
                                ada
                                deposit
                                (Pay
                                    charlie
                                    (Account bob)
                                    ada
                                    deposit
                                    Close 
                                )
                            )
                        )]
                    thirdDepositTimeout Close 
                )