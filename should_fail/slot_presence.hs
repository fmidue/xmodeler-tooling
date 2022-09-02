MLM
    { name = Name "slot_presence"
    , classes =
        [ Class
            { isAbstract = False
            , level = 2
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes =
                [ Attribute
                    { level = 1
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
            , level = 1
            , name = Name "B1"
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
                ]
            }
        , Class
            { isAbstract = False
            , level = 1
            , name = Name "B2"
            , parents =
                [ Name "B1" ]
            , classifier = Just
                ( Name "A" )
            , attributes = []
            , operations = []
            , slots = []
            }
        ]
    , associations = []
    , links = []
    }
