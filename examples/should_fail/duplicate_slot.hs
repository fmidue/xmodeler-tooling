MLM
    { name = Name "duplicate_slot"
    , classes =
        [ Class
            { isAbstract = False
            , level = 1
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes =
                [ Attribute
                    { level = 0
                    , name = Name "att"
                    , dataType = Integer
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots = []
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "a"
            , parents = []
            , classifier = Just
                ( Name "A" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { name = Name "att"
                    , value = VInteger 1
                    }
                , Slot
                    { name = Name "att"
                    , value = VInteger 2
                    }
                ]
            }
        ]
    , associations = []
    , links = []
    }
