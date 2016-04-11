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

professorAForm :: Maybe Professor -> AForm Handler Professor
professorAForm mprofessor = Professor
    <$> areq textField "Nome" (professorName <$> mprofessor)

courseAForm :: Maybe Course -> AForm Handler Course
courseAForm mcourse = Course
    <$> aopt textField "Nome" (courseName <$> mcourse)
    <*> areq textField "Codigo" (courseCode <$> mcourse)
    <*> areq faseField "Fase" (courseFase <$> mcourse)
    <*> aopt (selectField professors) "Professor" (courseProfessorId <$> mcourse)
      where professors =  optionsPersistKey [] [Asc ProfessorName] professorName


faseField = checkBool checkN errorMessage intField
  where errorMessage = "Fase Inválida" :: Text
        checkN n = (n > 0) && (n <= 10)

getListR :: Handler Html
getListR = do
  professors     <- runDB $ selectList [] [Asc ProfessorName]
  (profW, p_enc) <- generateFormPost $ renderDivs $ professorAForm $ Nothing

  courses          <- runDB $ selectList [] [Asc CourseCode]
  courses_prof <- mapM getProf courses

  (courseW, c_enc) <- generateFormPost $ renderDivs $ courseAForm $ Nothing

  fases <- runDB $ selectList [] [Asc FaseNumber, Asc FaseTurn]

  defaultLayout $ do
    setTitle "Professors"
    $(widgetFile "professors" )

 where
   getProf (Entity k c) = do
     mprof <- case courseProfessorId c of
                    Just pId -> runDB $ get pId
                    Nothing -> return Nothing
     return (k, c, mprof)



postNewProfessorR :: Handler Html
postNewProfessorR =
  do
    ((res, _), _) <- runFormPost $ renderDivs $ professorAForm $ Nothing
    case res of
      FormSuccess prof -> runDB $ insert_ prof
      _                -> print $ ("Error" :: Text)
    redirect ListR

postDeleteProfessorR :: Text -> Handler Html
postDeleteProfessorR pName =
  do runDB $ deleteBy $ UniqueProfessor pName
     redirect ListR


postNewCourseR :: Handler Html
postNewCourseR =
  do
    ((res, _), _) <- runFormPost $ renderDivs $ courseAForm $ Nothing
    case res of
      FormSuccess course -> runDB $ insert_ course
      _                  -> print $ ("Error" :: Text)
    redirect ListR

postDeleteCourseR :: Text -> Handler Html
postDeleteCourseR cCode =
  do runDB $ deleteBy $ UniqueCourse cCode
     redirect ListR


postSlotR :: Handler Html
postSlotR =
  do runDB $ deleteWhere ([] :: [Filter Slot])
     runDB $ deleteWhere ([] :: [Filter Fase])
     runDB $ mapM_ (\(n, t) -> insert_ $ Fase n t False) [(n, t) | n <- [1..10], t <- [1, 2]]
     return "Deleting all slots\n"

postFaseR :: Key Fase -> Handler Html
postFaseR k =
  do mfase  <- runDB $ get k
     case mfase of
       Just f -> runDB $ replace k $ f { faseValid = not $ faseValid f }
       Nothing -> print $ ("Error" :: Text)
     redirect ListR

