#WM_CAP_START = #WM_USER
#WM_CAP_DRIVER_CONNECT = #WM_CAP_START + 10
#WM_CAP_DRIVER_DISCONNECT = #WM_CAP_START + 11
#WM_CAP_DRIVER_GET_CAPS = #WM_CAP_START + 14
#WM_CAP_EDIT_COPY = #WM_CAP_START + 30
#WM_CAP_SET_PREVIEW = #WM_CAP_START + 50
#WM_CAP_SET_PREVIEWRATE = #WM_CAP_START + 52
#WM_CAP_STOP = #WM_CAP_START + 68
#WM_CAP_SET_SCALE = #WM_CAP_START + 53

#WM_CAP_DLG_VIDEOFORMAT = #WM_CAP_START + 41
#WM_CAP_DLG_VIDEOSOURCE = #WM_CAP_START + 42
#WM_CAP_DLG_VIDEODISPLAY = #WM_CAP_START + 43
#WM_CAP_GET_VIDEOFORMAT = #WM_CAP_START + 44
#WM_CAP_SET_VIDEOFORMAT = #WM_CAP_START + 45
#WM_CAP_DLG_VIDEOCOMPRESSION = #WM_CAP_START + 46

XIncludeFile "NearestColorModule.pbi"


UsePNGImageDecoder()
UseJPEGImageDecoder()

UseModule NearestColor

Structure HistogramType
  Pwin.i ; parent window number
  Hwin.i ; histogram window number
  Himg.i ; source image number of histogram
  R.i[256]
  G.i[256]
  B.i[256]
  maxR.i
  maxG.i
  maxB.i
  ChkR.i  ; red checkbox gadget number
  ChkG.i  ; green checkbox gadget number
  ChkB.i  ; blue checkbox gadget number
  ChkEx.i ; exclude checkbox gadget number
EndStructure

Global Hist.HistogramType
Hist\Hwin  = -1

Declare HISTOGRAM_EVENT_HANDLER()
Enumeration     ;User Interface
  #WINDOW_MAIN
  #WinPalette
  #SEARCH_IMAGE
  #UPLOAD_BUTTON
  #IMAGE_ADDRESS
  #CAPTURE_BUTTON
  #WEBCAM_FRAME
  #CONNECT_WEBCAM
  #CLOSE_WEBCAM
  #RESULT_FRAME
  #TAGS
  #EDIT_TAGS
  #SAVE_TAGS
  #PALETTE_FRAME
  #ID_Check
  #FURN_Check
  #DOOR_Check
  #WINDOW_Check
  #RESULT_AREA
  #Last_gadget
EndEnumeration

Enumeration 4 ; images
  #ImgMaster  : #ImgArrow_R : #ImgArrow_L : #ImgHist : #ImgTemp : #ImgSel
  #imgPalette
  #PALIMGAD
EndEnumeration

Declare POPULARITY_PALETTE(ImgRef.i,limit.i)
Declare SCAN_FOR_PALETTE(image.i)
Declare SHOW_PALETTE(Array palette.l(1))
Declare COUNT_COLORS(image.i)
Declare ASSEMBLE_TO_PALETTE(Array palette.l(1), ImgRef.i, dither.i)
Declare ExtractPalette()
Declare ShowImage()
Declare SetGadgetFontStyle(gad,bold,italic,underline,strikeout)

Declare COUNT_COLORS(image.i)
Declare LOAD_IMAGE()


#Mwin = 0
#image = 0
#Status = 0

; gadgets
Enumeration
  #Btn_Load : #Canvas : #ScrollArea : #CC : #Chk_Hist
EndEnumeration


Structure PopType ; used in POPULARITY_PALETTE()
  clr.l           ; color
  pop.i           ; popularity
EndStructure 

Global Dim WK_Pal.l(0) ;POPULARITY_PALETTE() fills this array

Define.i w,h,Qimage
Define.i limit = 72 ; the maximum number of colors in the result
Define.i dither = #True
Define.i ImgRef

Define.b tagList
Define.b connectCam
Define.b editButton
Define.i hCapWnd
Define.b firstRun

Import "avicap32.lib"
  CapCreateCaptureWindow.i(name.s, style.i, x.i, y.i, width.i, height.i, hWndParent.i, nId.i) As "_capCreateCaptureWindowA@32"
  ;CapCreateCaptureWindow("Captured Image", #WS_CHILD | #WS_VISIBLE,25,25,320,240,WindowID(0),0)
  CapGetDriverDescription.i(index.i, name.i, cbName.i, ver.i, cbVer.i) As "_capGetDriverDescriptionA@20"
EndImport

Procedure ImageToMatrix(Image,Array P(2))
  Protected Width= ImageWidth(0)-1, Height= ImageHeight(0)-1, x, y
  ; Scaling down Width & Height by -1 to compensate for using 0-based arrays
  Dim P(Width,Height)
  StartDrawing(ImageOutput(Image))
  For x=0 To Width
    For y=0 To Height
      P(x,y)=Point(x,y)
    Next y
  Next x
  StopDrawing()
EndProcedure

Procedure DISPLAY_HISTOGRAM(image.i = -1)
  ; call this procedure with no parameter to close the histogram window
  Static igad, HisImg
  Static flags =  #PB_Window_SystemMenu
  Protected h = 128 ;graph height
  Protected yScale.f,Xmax,Ymax,c,r,g,b,n,x,y,rc,gc,bc
  
  If image < 0
    If IsWindow(Hist\Hwin) <> 0
      FreeImage(HisImg)
      CloseWindow(Hist\Hwin)
      Hist\Hwin = -1
    EndIf
    ProcedureReturn #True
  EndIf
  
  If IsImage(image)
    Hist\Himg = image
    
    If IsWindow(Hist\Hwin) = #False
      
      Hist\Hwin = OpenWindow(#PB_Any,WindowX(#WINDOW_MAIN,#PB_Window_InnerCoordinate)-350,WindowY(#WINDOW_MAIN,#PB_Window_InnerCoordinate),340,h+10,"Histogram",flags,WindowID(#WINDOW_MAIN))
      If Hist\Hwin = 0 : ProcedureReturn #False : EndIf
      
      igad = ImageGadget(#PB_Any,5,5,256,h,0)
      Hist\ChkR  = CheckBoxGadget(#PB_Any, 270,010,50,25,"RED")
      Hist\ChkG  = CheckBoxGadget(#PB_Any, 270,040,50,25,"GRN")
      Hist\ChkB  = CheckBoxGadget(#PB_Any, 270,070,50,25,"BLU")
      Hist\ChkEx = CheckBoxGadget(#PB_Any,270,100,50,25,"ExEx")
      GadgetToolTip(Hist\ChkEx,"Exclude Extremes")
      
      SetGadgetState(Hist\ChkR,#True)
      SetGadgetState(Hist\ChkG,#True)
      SetGadgetState(Hist\ChkB,#True)
      
      BindGadgetEvent(Hist\ChkR, @HISTOGRAM_EVENT_HANDLER())
      BindGadgetEvent(Hist\ChkG, @HISTOGRAM_EVENT_HANDLER())
      BindGadgetEvent(Hist\ChkB, @HISTOGRAM_EVENT_HANDLER())
      BindGadgetEvent(Hist\ChkEx,@HISTOGRAM_EVENT_HANDLER())
      
      HisImg = CreateImage(#PB_Any,256,h)
    EndIf
    
    ;{ COMPUTE HISTOGRAM ****
    With Hist
      For x = 0 To 255 : \R[x]=0 : \G[x]=0 : \B[x]=0 : Next x
      \maxR = 0 : \maxG = 0 : \maxB = 0
      
      StartDrawing(ImageOutput(image))
      Xmax = OutputWidth() - 1
      Ymax = OutputHeight() - 1
      
      For y = 0 To Ymax
        For x = 0 To Xmax
          c = Point(x, y)
          \R[c & $FF] + 1 : c >> 8
          \G[c & $FF] + 1 : c >> 8
          \B[c & $FF] + 1
        Next x
      Next y
      
      If GetGadgetState(\ChkEx) ; exclude bins 0 and 255
        For x = 1 To 254
          If \R[x] > \maxR : \maxR = \R[x] : EndIf
          If \G[x] > \maxG : \maxG = \G[x] : EndIf
          If \B[x] > \maxB : \maxB = \B[x] : EndIf
        Next
      Else
        For x = 0 To 255
          If \R[x] > \maxR : \maxR = \R[x] : EndIf
          If \G[x] > \maxG : \maxG = \G[x] : EndIf
          If \B[x] > \maxB : \maxB = \B[x] : EndIf
        Next
      EndIf
      StopDrawing()
    EndWith : ;} END COMPUTE HISTOGRAM ****
    
    StartDrawing(ImageOutput(HisImg))
    Box(0,0,256,h,0)
    DrawingMode(#PB_2DDrawing_XOr)
    
    If GetGadgetState(Hist\ChkR) : rc = $0000FF
      yScale = Hist\maxR
    EndIf
    If GetGadgetState(Hist\ChkG) : gc = $00FF00
      If Hist\maxG > yScale : yScale = Hist\maxG : EndIf
    EndIf
    If GetGadgetState(Hist\ChkB) : bc = $FF0000
      If Hist\maxB > yScale : yScale = Hist\maxB : EndIf
    EndIf
    
    yScale = h / yScale : h - 1
    
    For x = 0 To 255
      LineXY(x, h, x, h - Hist\R[x] * yScale, rc)
      LineXY(x, h, x, h - Hist\G[x] * yScale, gc)
      LineXY(x, h, x, h - Hist\B[x] * yScale, bc)
    Next x
    StopDrawing()
    
    SetGadgetState(igad,ImageID(HisImg))
  EndIf
  
  ProcedureReturn #True
EndProcedure

Procedure HISTOGRAM_EVENT_HANDLER()
  DISPLAY_HISTOGRAM(Hist\Himg)
EndProcedure


Procedure POPULARITY_PALETTE(ImgRef.i,limit.i)
  ; Create a color palette with a modified popularity approach.
  ; ImgRef = the source image.
  ; Limit = the maximum number of colors in result.
  ; Limit count can be specified from 2 to 256. (256 is arbitrary limit)
  ; Required support procedures are:
  ;   'COUNT_COLORS', and 'ASSEMBLE_TO_PALETTE'.
  ; Finished palette is placed in the global array WK_Pal().
  ; This algorithm was created by BasicallyPure.
  
  Static.i kb = $FF0000, kg = $00FF00, kr = $0000FF
  Protected.i ImgWork, count, Xmin,Ymin,Xmax, Ymax, i, x, y, lum, d, br, da, mb, md
  
  Macro BMSK(mask) ; simple bitmask color reduction method
    StartDrawing(ImageOutput(ImgWork))
    For y = Ymin To yMax
      For x = Xmin To xMax
        Plot(x, y, Point(x,y) & mask | $0F0F0F)
      Next x
    Next y
    StopDrawing()
  EndMacro
  
  If IsImage(ImgRef)
    ImgWork = CopyImage(ImgRef,#PB_Any)
    
    If ImgWork
      count = COUNT_COLORS(ImgWork)
      If count <= limit : limit = count : EndIf
    Else
      ProcedureReturn 0
    EndIf
  Else
    ProcedureReturn 0
  EndIf
  
  ; 1) preprocess colors using BitMask method, color count will
  ;    be greatly reduced.
  Xmax = ImageWidth(ImgWork) - 1
  Ymax = ImageHeight(ImgWork) - 1
  
  If limit < 48
    BMSK($C0C0C0) ; _64 possible
    count = COUNT_COLORS(ImgWork)
  Else
    count = 0
  EndIf
  
  If count < (limit + 10) ; need more colors to work with
    If IsImage(ImgWork) : FreeImage(ImgWork) : EndIf
    ImgWork = CopyImage(ImgRef,#PB_Any)
    BMSK($E0E0E0) ; _512 possible
    count = COUNT_COLORS(ImgWork)
    If count < (limit + 10) ; need still more colors
      If IsImage(ImgWork) : FreeImage(ImgWork) : EndIf
      ImgWork = CopyImage(ImgRef,#PB_Any)
      BMSK($F0F0F0) ; _4096 possible
    EndIf
  EndIf
  
  ; 2) gather popularity data
  NewMap Pmap.i()
  StartDrawing(ImageOutput(ImgWork))
  For y = Ymin To Ymax
    For x = Xmin To Xmax
      Pmap(Str(Point(x,y))) + 1
    Next 
  Next
  StopDrawing()
  
  If MapSize(Pmap()) < limit : limit = MapSize(Pmap()) : EndIf
  
  ; 3) subdivide colors into 4 brightness lists
  NewList bright.PopType()
  NewList MedBri.PopType()
  NewList MedDrk.PopType()
  NewList dark.PopType()
  
  ForEach Pmap()
    d = Val(MapKey(Pmap()))
    lum = (d & kr)<<1 + (d & kg) >> 6 + (d & kb) >> 16
    If lum > 1338
      AddElement(bright()) : bright()\clr = d : bright()\pop = Pmap()
    ElseIf lum > 892
      AddElement(MedBri()) : MedBri()\clr = d : MedBri()\pop = Pmap()
    ElseIf lum > 446
      AddElement(MedDrk()) : MedDrk()\clr = d : MedDrk()\pop = Pmap()
    Else
      AddElement(dark())   : dark()\clr   = d : dark()\pop   = Pmap()
    EndIf
  Next
  
  ; 4) sort each brightness lists by popularity
  SortStructuredList(bright(),#PB_Sort_Descending,OffsetOf(PopType\pop),#PB_Integer)
  SortStructuredList(MedBri(),#PB_Sort_Descending,OffsetOf(PopType\pop),#PB_Integer)
  SortStructuredList(MedDrk(),#PB_Sort_Descending,OffsetOf(PopType\pop),#PB_Integer)
  SortStructuredList(dark()  ,#PB_Sort_Descending,OffsetOf(PopType\pop),#PB_Integer)
  
  ; 5) create the final palette
  FirstElement(bright()) : br = ListSize(bright())
  FirstElement(MedBri()) : mb = ListSize(MedBri())
  FirstElement(MedDrk()) : md = ListSize(MedDrk())
  FirstElement(dark())   : da = ListSize(dark())
  
  limit - 1
  ReDim WK_Pal(limit)
  
  i = 0 : d = %00
  Repeat ; pick from each list in turn the most popular color
    If d = %00 And br > 0
      WK_Pal(i) = bright()\clr
      NextElement(bright())
      i + 1 : br - 1
    ElseIf d = %01 And da > 0
      WK_Pal(i) = dark()\clr
      NextElement(dark())
      i + 1 : da - 1
    ElseIf d = %10 And mb > 0
      WK_Pal(i) = MedBri()\clr
      NextElement(MedBri())
      i + 1 : mb - 1
    ElseIf d = %11 And md > 0
      WK_Pal(i) = MedDrk()\clr
      NextElement(MedDrk())
      i + 1 : md - 1
    EndIf
    
    d = (d + %01) & %11
  Until i > limit
  
  If IsImage(ImgWork) : FreeImage(ImgWork) : EndIf
  
  ProcedureReturn 1
EndProcedure

Procedure COUNT_COLORS(image.i)
  ; returns the number of unique colors in an image (24 bit)
  Protected.i x, y, max_x, max_y, c, count, m
  Dim m.a($1FFFFF)
  StartDrawing(ImageOutput(image))
  max_x = ImageWidth(image) - 1
  max_y = ImageHeight(image) - 1
  For y = 0 To max_y
    For x = 0 To max_x
      c = Point(x, y) & $FFFFFF
      If m(c >> 3) & 1 << (c & 7) = 0
        m(c >> 3) | 1 << (c & 7)
        count + 1
      EndIf
    Next
  Next
  StopDrawing()
  ProcedureReturn count
EndProcedure

Procedure ASSEMBLE_TO_PALETTE(Array palette.l(1), ImgRef.i, dither.i)
  ; assign each pixel of an image to the defined palette using NearestColor module
  ; ImgRef  = the source image
  ; dither: 0 = no dither, 1 = dither
  ; A new image is created, the return value is the new image number.
  
  NearestColor::CatchPalette(@palette(), ArraySize(palette())+1)
  
  ProcedureReturn NearestColor::DitheredImage(ImgRef, dither*128)
  
EndProcedure

Procedure SCAN_FOR_PALETTE(image.i)
  ; obtain the palette of all colors used in an image
  ; stops if number of colors exceeds 288
  Static NewMap Pmap.i(1024)
  Static Dim Palette.l(0)
  Protected c,i,x,y,Xmax,Ymax
  
  Xmax = ImageWidth(image)-1
  Ymax = ImageHeight(image)-1
  
  StartDrawing(ImageOutput(image))
  For y = 0 To Ymax
    For x = 0 To Xmax
      c = Point(x,y)
      If MapSize(Pmap()) > 288
        Break 2
      EndIf
      Pmap(Str(c)) = c
    Next
  Next
  StopDrawing()
  
  ReDim palette(MapSize(Pmap())-1)
  
  i = 0
  ForEach Pmap()
    palette(i) = Pmap()
    i + 1
  Next
  
  ClearMap(Pmap())
  
  SHOW_PALETTE(Palette())
EndProcedure

Procedure SHOW_PALETTE(Array palette.l(1))
  ; draw the palette window
  
  Static flags = #PB_Window_SystemMenu | #PB_Window_Tool | #PB_Window_ScreenCentered
  Static imgGad, imgPalette
  Protected c, i, x, y, columns, Xmax, Ymax, blockSize
  Protected Imax = ArraySize(palette())
  
  If Imax > 287 ; (12_columns * 24_rows) - 1
    Imax = 287
  EndIf
  
  columns = 8
  blockSize = 320 / columns
  Xmax = columns * blockSize
  Ymax = Xmax - 120
  
  If IsImage(imgPalette) = 0
    imgPalette = CreateImage(#PB_Any,Xmax,Ymax,24,0)
  Else
    ResizeImage(imgPalette,Xmax,Ymax)
  EndIf
  
  StartDrawing(ImageOutput(imgPalette))
  
  Xmax - 1 : Ymax - 1
  For y = 0 To Ymax
    For x = 0 To Xmax
      c = (x ! y)
      c = (c << 16) | (c << 8) | c
      Plot(x, y, c | $C0C0C0)
    Next
  Next
  X = 0 : Y = 0
  For I = 0 To Imax
    Box(X, Y, blockSize, blockSize, palette(I))
    X + blockSize
    If X > Xmax : X = 0 : Y + blockSize : EndIf
  Next
  StopDrawing()
  
  ;imgGad = ImageGadget(#PB_Any,25,320,ImageWidth(imgPalette),ImageHeight(imgPalette),ImageID(imgPalette))
  SetGadgetState(#PALIMGAD,ImageID(imgPalette))  
  ;SetGadgetState(#PALETTE_FRAME,ImageID(imgPalette))  
  
EndProcedure

;Capture photo from webcam and saved to Pictures folder
Procedure CapImage()
  Static sct
  Define.i ImgRef
  Define.i limit = 72 
  Define.i w,h,Qimage
  Define.i dither = #True
  
  Image_ref.s="C:\Users\DELL\Desktop\리오\CBIR\webcam\photoYYH_00" + Str(sct) +".bmp"   
  ImgRef = GetClipboardImage(#PB_Any,32)
  
  ;Debug ImgRef
  If ImgRef
    
    ;     Image_ref.s="C:\Users\DELL\Desktop\리오\CBIR\webcam\photoYYH_00" + Str(sct) +".bmp"    
    SaveImage(ImgRef, Image_ref) 
    sct+1
    ; EndIf
    
    ; If ImgRef
    If POPULARITY_PALETTE(ImgRef, limit) ;generate the palette
      Qimage = ASSEMBLE_TO_PALETTE(WK_Pal(), ImgRef, dither) ;build the final image
      If IsImage(Qimage)
        SCAN_FOR_PALETTE(Qimage) ; show the palette 
      EndIf
    EndIf  
    ; EndIf
    
    If ImageWidth(ImgRef) > 320 Or ImageHeight(ImgRef) > 240
      If ImageWidth(ImgRef) > ImageHeight(ImgRef)
        ResizeImage(ImgRef, 320, 240)
      Else
        ResizeImage(ImgRef, ImageWidth(ImgRef)/2, ImageHeight(ImgRef)/2)
      EndIf
    EndIf
    
    If OpenWindow(#ImgSel,WindowX(#WINDOW_MAIN,#PB_Window_InnerCoordinate) - ImageWidth(ImgRef) ,WindowY(#WINDOW_MAIN,#PB_Window_InnerCoordinate),ImageWidth(ImgRef),ImageHeight(ImgRef),"Uploaded Image")
      ImageGadget(#ImgSel,0,0,0,0,ImageID(ImgRef))
      
    EndIf
    ;EndIf  
    ;   
    ;------------------------------------------------
    
    Define File2$, totalDiff=0, x, y, w, h
    Define.f result =0
    Define.s address 
    Define.i FrameX = 10, FrameY = 10
    Define.i ImgX = 20, ImgY = 30
    ;Define.i Search =  15
    Define.i e = 1
    
    
    Dim photo.s(172)
    For a = 1 To ArraySize(photo())
      photo(a) = "image_id0"+Str(a)+".jpg"
    Next a
    
    
    
    Search.i = #Last_gadget
    
    FrameX = 10
    ImgX = 20
    count=1
    
    For k = 1 To 172
      File1$=Image_ref
      Debug File1$
      result=0
      totalDiff = 0
      File2$ = photo(k)
      address = File2$
      LoadImage(0,File1$):LoadImage(1,File2$)
      ResizeImage(0, 120,90)
      ResizeImage(1, 120,90)
      Dim Pic1(0,0): Dim Pic2(0,0)
      ImageToMatrix(0,Pic1()): ImageToMatrix(1,Pic2())
      
      ;Compare difference
      w=ArraySize(pic1()): h=ArraySize(pic1(),2)
      For x=0 To w
        For y=0 To h
          totalDiff+ Abs(  Red(Pic1(x,y)) -   Red(Pic2(x,y)))
          totalDiff+ Abs(Green(Pic1(x,y)) - Green(Pic2(x,y)))
          totalDiff+ Abs( Blue(Pic1(x,y)) -  Blue(Pic2(x,y)))
        Next y
      Next x
      
      result = 100 * totalDiff /(255*w*h*3)
      Debug k
      Debug result
      If  result <= 25
        
        OpenGadgetList(#RESULT_AREA)       
                    FrameGadget(Search, FrameX, FrameY, 140, 120, photo(k))
                    Search + 1
                    ImageGadget(Search, ImgX, ImgY, ImageWidth(1), ImageHeight(1), ImageID(1))
                    CloseGadgetList()
                    Search + 1
                    ;position
                    
                    FrameX = FrameX + 150
                    ImgX = ImgX + 150
                    count+1
                    
                    If count = 4
                      FrameX = 10
                      ImgX = 20 
                      FrameY = FrameY + 125
                      ImgY = ImgY + 125                         
                      count = 1
                      SetGadgetAttribute(#RESULT_AREA, #PB_ScrollArea_InnerHeight , FrameY+ImgY+10)
                      
                    EndIf
      EndIf
      
    Next k
    
    Debug "Finished"
  
    ;--------------------------------------------------
    
    
  EndIf
  
  
  
EndProcedure   

;USER INTERFACE
If OpenWindow(#WINDOW_MAIN, 0,0,860,550, "I M A H E", #PB_Window_SystemMenu | #PB_Window_ScreenCentered | #PB_Window_Tool) 
  TextGadget(#PB_Any, 376, 10, 300, 20, "Capture/Upload an Image:")
  TextGadget(#PB_Any, 376, 55, 500, 20, "SEARCH RESULTS", #PB_Text_Center)
  StringGadget(#IMAGE_ADDRESS, 375, 25, 320, 20, "")
  ButtonGadget(#SEARCH_IMAGE, 700,25,75,20, "Search")
  ButtonGadget(#UPLOAD_BUTTON, 780, 25, 75,20, "Compare")
  
  ButtonGadget(#CONNECT_WEBCAM, 120, 270, 125, 20, "Connect Webcam") : connectCam = #True
  ButtonGadget(#CAPTURE_BUTTON,100,270,75,20,"Capture")
  ButtonGadget(#CLOSE_WEBCAM,175,270,78,20,"Disconnect")
  HideGadget(#CAPTURE_BUTTON,1)
  HideGadget(#CLOSE_WEBCAM,1)
  FrameGadget(#PALETTE_FRAME, 10, 300, 350, 240, "Color Palette")
  ImageGadget(#PALIMGAD,25,320,0,0,0)
  FrameGadget(#WEBCAM_FRAME, 10, 10, 350, 285, "Microsoft LiveCam")
  ScrollAreaGadget(#RESULT_AREA, 375, 75, 480,465, 450, 450,1000, #PB_ScrollBar_Vertical)
  CloseGadgetList()
  hCapWnd= CapCreateCaptureWindow("Captured Image", #WS_CHILD | #WS_VISIBLE,25,25,320,240,WindowID(0),0)
  
  Repeat
    Event = WaitWindowEvent()
    Select EventWindow()
        
      Case #WINDOW_MAIN
        
        Select Event
          Case #PB_Event_Gadget
            Select EventGadget()
              Case #CONNECT_WEBCAM  ;Connect to Webcam event
                connectCam = #False
                HideGadget(#CONNECT_WEBCAM, 1)
                HideGadget(#CAPTURE_BUTTON,0)
                HideGadget(#CLOSE_WEBCAM,0)
                SendMessage_(hCapWnd, #WM_CAP_DRIVER_CONNECT, 0, 0)
                SendMessage_(hCapWnd, #WM_CAP_SET_PREVIEW, 1, 0)
                SendMessage_(hCapWnd, #WM_CAP_SET_SCALE,1,0)
                SendMessage_(hCapWnd, #WM_CAP_SET_PREVIEWRATE, 10, 0)
                
              Case #CAPTURE_BUTTON
                SendMessage_(hCapWnd, #WM_CAP_EDIT_COPY, 0, 0)
                PostMessage_(hCapWnd,#WM_CAP_EDIT_COPY,0,0)
                CapImage()                
                
              Case #CLOSE_WEBCAM    ;Disconnect Webcam event
                connectCam= #True
                HideGadget(5,1)
                HideGadget(8,1)
                HideGadget(7,0)
                SendMessage_(hCapWnd, #WM_CAP_STOP, 0, 0)
                SendMessage_(hCapWnd,#WM_CAP_DRIVER_DISCONNECT,0,0)
                
              Case #SEARCH_IMAGE    
                Define Pattern$ = "image (*.png, *.jpg, *.bmp)|*.png;*.jpg;*.bmp|image *.*|*.*"
                Define FileName$
                Define File1$ = OpenFileRequester("Select image to process", FileName$, Pattern$, 0)
                SetGadgetText(#IMAGE_ADDRESS, File1$) 
                If File$
                  Define SourceFileName$ = GetFilePart(File$, #PB_FileSystem_NoExtension)       
                  If LoadImage(#ImgTemp,File1$)   
                    CopyImage(#ImgTemp,ImgRef)
                    FreeImage(#ImgTemp)
                  EndIf
                EndIf
                
              Case #EDIT_TAGS
                tagList = #False
                editButton = #False
                DisableGadget(11,0)
                HideGadget(12,1)
                ButtonGadget(#SAVE_TAGS, 725, 445, 65, 20, "Save")
                
              Case #SAVE_TAGS
                tagList = #True
                editButton = #True
                HideGadget(12,0)
                
              Case #UPLOAD_BUTTON
                Define File2$, totalDiff=0, x, y, w, h
                Define.f result =0
                Define.s address 
                Define.i FrameX = 10, FrameY = 10
                Define.i ImgX = 20, ImgY = 30
                Define.i Search = #Last_gadget

                
                ImgRef = LoadImage(#PB_Any,File1$)
                If ImgRef
                  If POPULARITY_PALETTE(ImgRef, limit) ;generate the palette
                    Qimage = ASSEMBLE_TO_PALETTE(WK_Pal(), ImgRef, dither) ;build the final image
                    If IsImage(Qimage)
                      SCAN_FOR_PALETTE(Qimage) ; show the palette
                                               ;DISPLAY_HISTOGRAM(ImgRef)
                    EndIf
                  EndIf                  
                EndIf 
                
                Dim photo.s(172)
                For a = 1 To ArraySize(photo())
                  photo(a) = "image_id0"+Str(a)+".jpg"
                Next a
                
                FrameX = 10
                ImgX = 20
                count=1
                OpenGadgetList(#RESULT_AREA) 
                For k = 1 To 172
                  
                  result=0
                  totalDiff = 0
                  File2$ = photo(k)
                  address = File2$
                  LoadImage(0,File1$):LoadImage(1,File2$)
                  ResizeImage(0, 120,90)
                  ResizeImage(1, 120,90)
                  Dim Pic1(0,0): Dim Pic2(0,0)
                  ImageToMatrix(0,Pic1()): ImageToMatrix(1,Pic2())
                  
                  ;Compare difference
                  w=ArraySize(pic1()): h=ArraySize(pic1(),2)
                  For x=0 To w
                    For y=0 To h
                      totalDiff+ Abs(  Red(Pic1(x,y)) -   Red(Pic2(x,y)))
                      totalDiff+ Abs(Green(Pic1(x,y)) - Green(Pic2(x,y)))
                      totalDiff+ Abs( Blue(Pic1(x,y)) -  Blue(Pic2(x,y)))
                    Next y
                  Next x
                  
                  result = 100 * totalDiff /(255*w*h*3)
                  ;Debug k
                  ;Debug result
                  If  result <= 25
                    
                    
                   
                    FrameGadget(Search, FrameX, FrameY, 140, 120, photo(k))
                    Search + 1
                    ImageGadget(Search, ImgX, ImgY, ImageWidth(1), ImageHeight(1), ImageID(1))
                    
                    Search + 1
                    ;position
                    
                    FrameX = FrameX + 150
                    ImgX = ImgX + 150
                    count+1
                    
                    If count = 4
                      FrameX = 10
                      ImgX = 20 
                      FrameY = FrameY + 125
                      ImgY = ImgY + 125                         
                      count = 1
                       FrImgy = FrameY+ImgY
                      SetGadgetAttribute(#RESULT_AREA, #PB_ScrollArea_InnerHeight , FrImgy)
                      
                    EndIf
                    
                  EndIf 

                Next k
                
                CloseGadgetList()               

                Debug "Finished"
                
            EndSelect
            
          Case #PB_Event_CloseWindow
            Exit = #True
            
            
          Case #ImgSel
            
            Select Event
              Case #PB_Event_CloseWindow
                CloseWindow(#ImgSel)
                
            EndSelect
            
        EndSelect
    EndSelect
    
  Until Exit
  
EndIf


; IDE Options = PureBasic 5.31 (Windows - x86)
; CursorPosition = 586
; FirstLine = 204
; Folding = kw
; EnableXP