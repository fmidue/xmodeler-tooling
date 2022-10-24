MLM
    { name = Name "inheritanceAt0"
    , classes =
        [ Class
            { isAbstract = False
            , level = 0
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes = []
            , operations = []
            , slots = []
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "B"
            , parents =
                [ Name "A" ]
            , classifier = Nothing
            , attributes = []
            , operations = []
            , slots = []
            }
        ]
    , associations = []
    , links = []
    }
