module Handler.Professor where

import Import

faseAForm :: Maybe Fase -> AForm Handler Fase
faseAForm mfase = Fase
    <$> areq hiddenField "" (faseNumber <$> mfase)
    <*> areq hiddenField "" (faseTurn <$> mfase)
    <*> areq hiddenField "" (faseValid <$> mfase)
  --where turnField = radioFieldList [("Matutino" :: Text, 1), ("Noturno", 2)]

--faseKeyAForm (Key kf = Key Fase
    --Key <$> areq hiddenField "FaseKey" (

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

--renderFormFase :: Entity Fase -> Handler Html
generateFormFase (Entity _ f) = do
  (w, e) <- generateFormPost $ renderTable $ faseAForm $ Just f
  return (f, w, e)

getListR :: Handler Html
getListR = do
  professors     <- runDB $ selectList [] [Asc ProfessorName]
  (profW, p_enc) <- generateFormPost $ renderDivs $ professorAForm
  courses          <- runDB $ selectList [] [Asc CourseCode]
  (courseW, c_enc) <- generateFormPost $ renderDivs $ courseAForm
  fases <- runDB $ selectList [] [Asc FaseNumber, Asc FaseTurn]
  f_ws  <- mapM generateFormFase fases
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

postFaseR :: Handler Html
postFaseR =
  do
    ((res, _), _) <- runFormPost $ renderTable $ faseAForm Nothing
    case res of
      FormSuccess f -> (runDB $ do
        Just (Entity k _) <- getBy $ UniqueFase (faseNumber f) (faseTurn f)
        replace k (Fase (faseNumber f) (faseTurn f) (not $ faseValid f))
        )
      _ -> print $ ("Error" :: Text)
    redirect ListR

