MLM
    { name = Name "differentLevels"
    , classes =
        [ Class
            { isAbstract = False
            , level = 4
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes =
                [ Attribute
                    { level = 3
                    , name = Name "another"
                    , dataType = Integer
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                , Attribute
                    { level = 2
                    , name = Name "attr"
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
            , level = 3
            , name = Name "B"
            , parents = []
            , classifier = Just
                ( Name "A" )
            , attributes =
                [ Attribute
                    { level = 1
                    , name = Name "attr"
                    , dataType = Integer
                    , multiplicity = Multiplicity
                        ( 1
                        , Just 1
                        )
                    }
                ]
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "another"
                    , value = VInteger 0
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 2
            , name = Name "C"
            , parents = []
            , classifier = Just
                ( Name "B" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "attr"
                    , value = VInteger 0
                    }
                ]
            }
        ]
    , associations = []
    , links = []
    }
