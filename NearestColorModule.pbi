; NearestColor module by Wilbert

; Last updated : Jan 26, 2015

; Color distance formula based on:
; http://www.compuphase.com/cmetric.htm

DeclareModule NearestColor
  
  Prototype ProtoProgressCallback(PercentProgress.i)
  
  Declare   CatchPalette(*MemoryAddress.Long, NumColors.i)
  Declare   CopyPalette(Array DestinationArray.l(1))
  Declare.i DitheredImage(Image.i, DitherLevel.a = 220, Brightness.b = 0, Contrast.b = 0, ProgressCallback.ProtoProgressCallback = 0)
  Declare.l FindNearest(Color.l)
  Declare.i PaletteColorCount()
  Declare   SetPalette(PaletteImage.i)
  
EndDeclareModule

Module NearestColor
  
  EnableASM
  EnableExplicit
  DisableDebugger
  
  Structure ColorScan
    l.l[524288]
  EndStructure
  
  Global ColorScan.ColorScan
  Global Dim IndexG.l(255)
  Global Dim Palette.l(1)
  
  CompilerIf #PB_Compiler_Processor = #PB_Processor_x86
    Macro rdx : edx : EndMacro
  CompilerEndIf
  
  Procedure.i PaletteColorCount()
    ; Return amount of colors the palette contains
    ProcedureReturn ArraySize(Palette()) - 1
  EndProcedure
  
  Procedure CopyPalette(Array DestinationArray.l(1))
    ; Copy the current palette into a supplied array
    Protected.i cnt = ArraySize(Palette()) - 1
    ReDim DestinationArray(cnt - 1)
    CopyMemory(@Palette(1), @DestinationArray(0), cnt << 2)
  EndProcedure
  
  Procedure CatchPalette(*MemoryAddress.Long, NumColors.i)
    ; Catch a palette from memory
    Protected.i i, j = 1
    ReDim Palette(NumColors + 1)
    Palette(0) = 0 : Palette(NumColors + 1) = 0
    For i = 1 To NumColors
      Palette(i) = $ff000000 | *MemoryAddress\l
      *MemoryAddress + 4
    Next
    SortStructuredArray(Palette(), 0, 0, #PB_Unicode, 1, NumColors)
    For i = 0 To 255
      IndexG(i) = j
      While ((Palette(j) >> 8) & $ff) = i
        j + 1
      Wend
      IndexG(i) = (IndexG(i) + j) >> 1
    Next
  EndProcedure
  
  Procedure SetPalette(PaletteImage.i)
    ; Set a palette from an image
    Protected.i i, j, b, x, y, cnt
    Protected.l c, c_
    If StartDrawing(ImageOutput(PaletteImage))
      FillMemory(@ColorScan, SizeOf(ColorScan))
      j = OutputHeight() - 1
      i = OutputWidth() - 1
      ; count all used colors
      For y = 0 To j
        For x = 0 To i
          c = Point(x, y)
          !mov eax, [p.v_c]
          !rol ax, 8
          !bswap eax
          !shr eax, 8
          lea rdx, [nearestcolor.v_ColorScan]
          bts [rdx], eax
          !jc nearestcolor.setpalette_cont0
          inc cnt
          !nearestcolor.setpalette_cont0:
        Next
      Next
      StopDrawing()
      ; redim palette with room at top and bottom for zero entry
      ReDim Palette(cnt + 1)
      Palette(0) = 0 : Palette(cnt + 1) = 0
      ; set palette sorted on G, R, B and index on G
      i = 0 : j = 1
      For y = 0 To 255
        IndexG(y) = j
        For x = 0 To 2047
          b = 0 : c_ = ColorScan\l[i]
          While c_
            shr c_, 1
            !jnc nearestcolor.setpalette_cont1          
            !mov eax, [p.v_i]
            !shl eax, 5
            !or eax, [p.v_b]
            !shl eax, 8
            !or eax, 0xff
            !bswap eax
            !rol ax, 8
            !mov [p.v_c], eax
            Palette(j) = c : j + 1
            !nearestcolor.setpalette_cont1:          
            b + 1
          Wend
          i + 1  
        Next
        IndexG(y) = (IndexG(y) + j) >> 1   
      Next
    EndIf  
  EndProcedure
  
  Macro M_FindNearest(i, st)
    !nearestcolor.findnearest#i#_loop: 
    !mov ecx, [p.v_c#i#]
    !test ecx, ecx
    !jz nearestcolor.findnearest#i#_cont2
    !movzx eax, byte [p.v_Color + 1]
    !movzx ecx, ch
    !sub eax, ecx
    !imul eax, eax
    !shl eax, 11
    !cmp eax, [p.v_bestd]
    !jnc nearestcolor.findnearest#i#_cont1
    !mov [p.v_d], eax
    !movzx eax, byte [p.v_Color]
    !movzx ecx, byte [p.v_c#i#]
    !lea edx, [eax + ecx]   ; edx = rsum
    !sub eax, ecx
    !imul eax, eax          ; eax = r*r
    !lea ecx, [edx + 0x400] ; ecx = $400 + rsum
    !imul eax, ecx          ; eax = ($400+rsum)*r*r
    !add [p.v_d], eax
    !movzx eax, byte [p.v_Color + 2]
    !movzx ecx, byte [p.v_c#i# + 2]
    !sub eax, ecx
    !imul eax, eax          ; eax = b*b
    !neg edx
    !add edx, 0x5fe         ; edx = $5fe - rsum
    !imul eax, edx          ; eax = ($5fe-rsum)*b*b
    !add eax, [p.v_d]
    !cmp eax, [p.v_bestd]
    !jnc nearestcolor.findnearest#i#_cont0
    !mov [p.v_bestd], eax
    !mov eax, [p.v_c#i#]
    !mov [p.v_c], eax
    !nearestcolor.findnearest#i#_cont0:
    mov rdx, *p#i
    add rdx, st
    mov *p#i, rdx
    mov eax, [rdx]
    !mov [p.v_c#i#], eax
    CompilerIf i = 1
      !jmp nearestcolor.findnearest0_loop
    CompilerElse
      !jmp nearestcolor.findnearest1_loop
    CompilerEndIf
    !nearestcolor.findnearest#i#_cont1:
    !mov dword [p.v_c#i#], 0
    !nearestcolor.findnearest#i#_cont2:
    CompilerIf i = 1
      !cmp dword [p.v_c0], 0
      !jnz nearestcolor.findnearest0_loop
    CompilerEndIf
  EndMacro
  
  Procedure.l FindNearest(Color.l)
    ; Find the nearest color
    Protected.l c, c0, c1, d, bestd = $12000000
    Protected.Long *p0, *p1
    !movzx eax, byte [p.v_Color + 1]
    !mov [p.v_d], eax
    *p1 = @Palette(IndexG(d)) : *p0 = *p1 - 4
    c0 = *p0\l : c1 = *p1\l
    M_FindNearest(0, -4)
    M_FindNearest(1, 4)
    ProcedureReturn c
  EndProcedure
  
  Macro M_DitherImage(offset)
    !movsx ecx, byte [p.v_err + offset]
    !movsx eax, byte [p.v_err50 + offset]
    !add ecx, eax
    !imul ecx, edx
    !sar ecx, 8
    !movzx eax, byte [p.v_c0 + offset]
    !add eax, [p.v_badd]
    !imul eax, [p.v_cmul]
    !sar eax, 8
    !lea eax, [eax + ecx + 128]
    !neg ah
    !setz cl
    !neg cl
    !and al, cl
    !sar ah, 7
    !or al, ah
    !mov [p.v_c0 + offset], al
  EndMacro
  
  Procedure.i DitheredImage(Image.i, DitherLevel.a = 220, Brightness.b = 0, Contrast.b = 0, ProgressCallback.ProtoProgressCallback = 0)
    ; Return a dithered image
    ; DitherLevel : 0 - 255
    ; Brightness : -128 - 127
    ; Contrast : -128 - 127
    Protected.i result, x, y, w, h
    Protected.l c0, c1, badd, cmul, err50, err
    If ProgressCallback : ProgressCallback(0) : EndIf
    result = CopyImage(Image, #PB_Any)
    If result And StartDrawing(ImageOutput(result))
      h = OutputHeight()
      w = OutputWidth()
      If DitherLevel = 0 And Brightness = 0 And Contrast = 0
        While y < h
          x = 0
          While x < w
            Plot(x, y, FindNearest(Point(x, y)))
            x + 1
          Wend
          y + 1
          If ProgressCallback
            ProgressCallback(100 * y / h)
          EndIf
        Wend
      Else
        badd = Brightness - 128
        cmul = (33280 * Contrast + 4259840) / (16640 - Contrast << 7)
        Dim d_error.l(w)
        While y < h
          x = 0 : err50 = 0
          While x < w
            c0 = Point(x, y)
            ; add previous error
            err = d_error(x)
            !movzx edx, byte [p.v_DitherLevel]
            M_DitherImage(0)
            M_DitherImage(1)
            M_DitherImage(2)
            c1 = FindNearest(c0)
            Plot(x, y, c1)
            ; calculate 50% error
            !mov eax, [p.v_c0]
            !mov ecx, [p.v_c1]
            !mov edx, eax
            !not edx
            !and edx, ecx
            !and edx, 0x01010101
            !or eax, 0x01010101
            !and ecx, 0xfefefefe
            !sub eax, ecx
            !xor eax, 0x01010101
            !shr eax, 1
            !sub eax, edx
            !mov ecx, [p.v_err50]
            !mov [p.v_err50], eax
            ; mix with previous error
            !xor eax, 0x80808080
            !xor ecx, 0x80808080
            !mov edx, eax
            !and edx, ecx
            !and edx, 0x01010101
            !and eax, 0xfefefefe
            !and ecx, 0xfefefefe
            !add eax, ecx
            !shr eax, 1
            !add eax, edx
            !xor eax, 0x80808080
            !mov [p.v_err], eax
            d_error(x) = err
            x + 1
          Wend
          y + 1
          If ProgressCallback
            ProgressCallback(100 * y / h)
          EndIf
        Wend
      EndIf
      StopDrawing()
    EndIf
    ProcedureReturn result
  EndProcedure
  
  DataSection
    CGAPalette:
    Data.l $000000, $AA0000, $00AA00, $AAAA00, $0000AA, $AA00AA, $0055AA, $AAAAAA
    Data.l $555555, $FF5555, $55FF55, $FFFF55, $5555FF, $FF55FF, $55FFFF, $FFFFFF
  EndDataSection
  
  ; Set default palette
  CatchPalette(?CGAPalette, 16)
  
  
EndModule
; IDE Options = PureBasic 5.30 (Windows - x86)
; CursorPosition = 302
; Folding = --
; EnableXP