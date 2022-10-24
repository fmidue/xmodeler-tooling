MLM
    { name = Name "circularity"
    , classes =
        [ Class
            { isAbstract = False
            , level = 1
            , name = Name "A"
            , parents =
                [ Name "B" ]
            , classifier = Nothing
            , attributes = []
            , operations = []
            , slots = []
            }
        , Class
            { isAbstract = False
            , level = 1
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
