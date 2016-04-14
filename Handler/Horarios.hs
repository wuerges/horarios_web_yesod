module Handler.Horarios where

import Import
--import Data.Map ((!))


--hiddenFieldSettings :: FieldSettings String
                       --fromString "Some MSG"  { fsAttrs = [("class", "hfield")] }


slotAForm0 :: AForm Handler Slot
slotAForm0  = Slot
  <$> aopt hiddenField "" Nothing
  <*> areq hiddenField "" Nothing
  <*> areq hiddenField "" Nothing
  <*> areq hiddenField "" Nothing

slotAForm :: Slot -> AForm Handler Slot
slotAForm (Slot cId day hour fase) = Slot
  <$> aopt (selectField courses) fs (Just cId)
  <*> areq hiddenField "" (Just day)
  <*> areq hiddenField "" (Just hour)
  <*> areq hiddenField "" (Just fase)
 where
   fs = FieldSettings
          { fsLabel = "this is not used"
          , fsTooltip = Nothing
          , fsId = Nothing
          , fsName = Nothing
          , fsAttrs = [] -- [("style", "width: 80px; height: 30px;")]
          }
   courses =
     optionsPersistKey ([CourseFase ==. fase] ||. [CourseFase ==. 0]) [Asc CourseCode] courseCode

makeSlotForm :: Int -> Int -> Int -> Widget
makeSlotForm fn d h = do
  slot <- handlerToWidget $ runDB $ do mslot <- getBy $ UniqueSlot fn d h
                                       case mslot of
                                         Just (Entity _ v) -> return v
                                         Nothing -> return $ Slot Nothing d h fn

  (w, _) <- handlerToWidget $ generateFormPost $ renderDivsNoLabels $ slotAForm $ slot
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

postSlotR :: Int -> Int -> Int -> Handler Html
postSlotR pf pd ph = do
  ((res, _), _) <- runFormPost $ renderDivs $ slotAForm0
  mslot <- runDB $ getBy $ UniqueSlot pf pd ph
  case res of
    FormSuccess s ->
      case mslot of
        Just (Entity k _) -> runDB $ replace k s
        Nothing -> runDB $ insert_ s
    _ ->
      case mslot of
        Just (Entity k _) -> runDB $ delete k
        Nothing -> return ()
  redirect HorariosR

