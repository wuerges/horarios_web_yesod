module Handler.Professor where

import Import

faseAForm :: Maybe Fase -> AForm Handler Fase
faseAForm mfase = Fase
    <$> areq faseField "Fase" (faseNumber <$> mfase)
    <*> areq turnField "Turno" (faseTurn <$> mfase)
    <*> areq checkBoxField "Válido" (faseValid <$> mfase)
  where turnField = radioFieldList [("Matutino" :: Text, 1), ("Noturno", 2)]

professorAForm :: AForm Handler Professor
professorAForm = Professor
    <$> areq textField "Nome" Nothing

courseAForm :: AForm Handler Course
courseAForm = Course
    <$> aopt textField "Nome" Nothing
    <*> areq textField "Codigo" Nothing
    <*> areq faseField "Fase" Nothing


faseField = checkBool checkN errorMessage intField
  where errorMessage = "Fase Inválida" :: Text
        checkN n = (n > 0) && (n <= 10)


getListR :: Handler Html
getListR = do
  professors  <- runDB $ selectList [] [Asc ProfessorName]
  courses     <- runDB $ selectList [] [Asc CourseCode]
  fases       <- runDB $ selectList [] [Asc FaseNumber, Asc FaseTurn]
  (profW, p_enc) <- generateFormPost $ renderTable $ professorAForm
  (courseW, c_enc) <- generateFormPost $ renderTable $ courseAForm
  defaultLayout $ do
    setTitle "Professors"
    $(widgetFile "professors" )


postProfessorR :: Handler Html
postProfessorR =
  do
    --professors  <- runDB $ selectList [] [Asc ProfessorName]
    ((res, _), _) <- runFormPost $ renderTable $ professorAForm
    case res of
      FormSuccess prof -> runDB $ insert_ prof
      _                -> print $ ("Error" :: Text)
    redirect ListR

postCourseR :: Handler Html
postCourseR =
  do
    ((res, _), _) <- runFormPost $ renderTable $ courseAForm
    case res of
      FormSuccess course -> runDB $ insert_ course
      _                  -> print $ ("Error" :: Text)
    redirect ListR

postSlotR :: Handler Html
postSlotR =
  do runDB $ deleteWhere ([] :: [Filter Slot])
     runDB $ deleteWhere ([] :: [Filter Fase])
     runDB $ mapM_ (\(n, t) -> insert_ $ Fase n t False) [(n, t) | n <- [1..10], t <- [1, 2]]
     return "Deleting all slots\n"
