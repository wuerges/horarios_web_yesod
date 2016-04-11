module Handler.Horarios where

import Import
--import Data.Map ((!))

slotAForm :: Slot -> AForm Handler Slot
slotAForm (Slot cId day hour fase) = Slot
  <$> aopt (selectField courses) "" (Just cId)
  <*> areq hiddenField "" (Just day)
  <*> areq hiddenField "" (Just hour)
  <*> areq hiddenField "" (Just fase)

slotAForm :: Slot -> AForm Handler Slot
slotAForm (Slot cId day hour fase) = Slot
  <$> aopt (selectField courses) "" (Just cId)
  <*> areq hiddenField "" (Just day)
  <*> areq hiddenField "" (Just hour)
  <*> areq hiddenField "" (Just fase)

 where courses = optionsPersistKey [CourseFase ==. fase] [Asc CourseCode] courseCode

makeSlotForm :: Int -> Int -> Int -> Widget
makeSlotForm fn d h = do
  slot <- handlerToWidget $ runDB $ do mslot <- getBy $ UniqueSlot fn d h
                                       case mslot of
                                         Just (Entity _ v) -> return v
                                         Nothing -> return $ Slot Nothing d h fn

  (w, _) <- handlerToWidget $ generateFormPost $ renderDivs $ slotAForm $ slot
  w



getHorariosR :: Handler Html
getHorariosR = do
  diurno <- runDB $ selectList [FaseTurn ==. 1, FaseValid ==. True] [Asc FaseNumber]
  noturno <- runDB $ selectList [FaseTurn ==. 2, FaseValid ==. True] [Asc FaseNumber]

  let days = [1, 2, 3, 4, 5] :: [Int]
      diurnos = [7, 9] :: [Int]
      noturnos = [19, 21] :: [Int]

  defaultLayout $ do
    setTitle "Horários"
    $(widgetFile "horarios")

postSlotR :: Handler Html
postSlotR = do

  mslot <- runDB $ getBy $ UniqueSlot f day hor
  case mslot of
    Just (Entity k v) -> runDB $ replace k (Slot Nothing day hor f)
    Nothing -> runDB $ insert_ $ Slot Nothing day hor f
  redirect HorariosR

