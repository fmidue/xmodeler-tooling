MLM
    { name = Name "duplicate_attribute"
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
                , Attribute
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
        ]
    , associations = []
    , links = []
    }
