module Handler.Horarios where

import Import
import qualified Carga as C
import qualified Algo as A

--import Database.Persist.Sql

--data CargaJ = CargaJ { __fases :: [(Int, Turno)]
--                    , __profs :: [(Disc, Int)]
--                    , __precolors :: [Precolor] }
--
-- toCarga
-- solve




getCargaR :: Handler Html
getCargaR = do
  fases <- runDB $ selectList [FaseValid ==. True] []
  professors <- runDB $ selectList [] [Asc ProfessorName]
  courses <- runDB $ selectList [] [Asc CourseCode]
  precolors <- runDB $ selectList [SlotCourseId !=. Nothing] []
  let ps_cs = [(p, kc, c) |
            Entity kp p <- professors,
            Entity kc c <- courses,
            Just kp == courseProfessorId c
          ]
      _pres = [(fn, C.Hor d h, C.Disc pname dname) |
            Entity ks (Slot cId d h fn) <- precolors,
            (Professor pname, kc, Course _ dname c_fn _) <- ps_cs,
            Just kc == cId]

      _fs = [(n, C.numberTurno t) | Entity _ (Fase n t _) <- fases]
      _ps = [(C.Disc pname dname, fn) |
              (Professor pname, _, Course _ dname fn _) <- ps_cs]
      _c = C.CargaJ _fs (_ps ++ _ps) _pres
      _s = A.solve (C.toCarga _c)
  print ps_cs
  print fases
  print precolors
  print _s
  defaultLayout $ do
    setTitle "Carga"
    $(widgetFile "carga")



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

days :: [Int]
days = [1, 2, 3, 4, 5]

diurnos :: [Int]
diurnos = [7, 9]

noturnos :: [Int]
noturnos = [19, 21]

getHorariosR :: Handler Html
getHorariosR = do
  diurno <- runDB $ selectList [FaseTurn ==. 1, FaseValid ==. True] [Asc FaseNumber]
  noturno <- runDB $ selectList [FaseTurn ==. 2, FaseValid ==. True] [Asc FaseNumber]


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

