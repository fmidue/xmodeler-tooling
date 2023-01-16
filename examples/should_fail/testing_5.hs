MLM
    { name = Name "testing_5"
    , classes =
        [ Class
            { isAbstract = False
            , level = 2
            , name = Name "C"
            , parents = []
            , classifier = Nothing
            , attributes =
                [ Attribute
                    { level = 1
                    , name = Name "att1"
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
            , name = Name "A"
            , parents = []
            , classifier = Nothing
            , attributes = []
            , operations = []
            , slots = []
            }
        , Class
            { isAbstract = False
            , level = 1
            , name = Name "B"
            , parents = []
            , classifier = Just
                ( Name "C" )
            , attributes =
                [ Attribute
                    { level = 0
                    , name = Name "att2"
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
                    { attribute = Name "att1"
                    , value = VInteger 1
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 1
            , name = Name "D"
            , parents = []
            , classifier = Just
                ( Name "C" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "att1"
                    , value = VInteger 5
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 1
            , name = Name "BB"
            , parents =
                [ Name "B" ]
            , classifier = Just
                ( Name "C" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "att1"
                    , value = VInteger 2
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "a0"
            , parents = []
            , classifier = Just
                ( Name "A" )
            , attributes = []
            , operations = []
            , slots = []
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "d0"
            , parents = []
            , classifier = Just
                ( Name "D" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "att2"
                    , value = VInteger 3
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "b0"
            , parents = []
            , classifier = Just
                ( Name "B" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "att2"
                    , value = VInteger 3
                    }
                ]
            }
        , Class
            { isAbstract = False
            , level = 0
            , name = Name "b1"
            , parents = []
            , classifier = Just
                ( Name "BB" )
            , attributes = []
            , operations = []
            , slots =
                [ Slot
                    { attribute = Name "att2"
                    , value = VInteger 4
                    }
                ]
            }
        ]
    , associations =
        [ Association
            { name = Name "d"
            , source = Name "B"
            , target = Name "D"
            , levelSource = 0
            , levelTarget = 0
            , multSource = Multiplicity
                ( 2
                , Nothing
                )
            , multTarget = Multiplicity
                ( 0
                , Nothing
                )
            , visibleSource = False
            , visibleTarget = True
            }
        , Association
            { name = Name "c"
            , source = Name "D"
            , target = Name "C"
            , levelSource = 0
            , levelTarget = 0
            , multSource = Multiplicity
                ( 0
                , Nothing
                )
            , multTarget = Multiplicity
                ( 0
                , Nothing
                )
            , visibleSource = False
            , visibleTarget = True
            }
        , Association
            { name = Name "a"
            , source = Name "A"
            , target = Name "B"
            , levelSource = 0
            , levelTarget = 0
            , multSource = Multiplicity
                ( 0
                , Just 2
                )
            , multTarget = Multiplicity
                ( 2
                , Nothing
                )
            , visibleSource = False
            , visibleTarget = True
            }
        , Association
            { name = Name "e"
            , source = Name "A"
            , target = Name "D"
            , levelSource = 0
            , levelTarget = 0
            , multSource = Multiplicity
                ( 0
                , Just 2
                )
            , multTarget = Multiplicity
                ( 0
                , Nothing
                )
            , visibleSource = False
            , visibleTarget = True
            }
        , Association
            { name = Name "b"
            , source = Name "C"
            , target = Name "A"
            , levelSource = 0
            , levelTarget = 0
            , multSource = Multiplicity
                ( 1
                , Just 2
                )
            , multTarget = Multiplicity
                ( 0
                , Just 1
                )
            , visibleSource = False
            , visibleTarget = True
            }
        ]
    , links =
        [ Link
            { association = Name "b"
            , source = Name "b0"
            , target = Name "a0"
            }
        , Link
            { association = Name "b"
            , source = Name "b1"
            , target = Name "a0"
            }
        , Link
            { association = Name "e"
            , source = Name "a0"
            , target = Name "d0"
            }
        , Link
            { association = Name "d"
            , source = Name "b0"
            , target = Name "d0"
            }
        , Link
            { association = Name "d"
            , source = Name "b1"
            , target = Name "d0"
            }
        , Link
            { association = Name "a"
            , source = Name "a0"
            , target = Name "b0"
            }
        , Link
            { association = Name "c"
            , source = Name "d0"
            , target = Name "b0"
            }
        , Link
            { association = Name "a"
            , source = Name "a0"
            , target = Name "b1"
            }
        , Link
            { association = Name "c"
            , source = Name "d0"
            , target = Name "b1"
            }
        ]
    }
