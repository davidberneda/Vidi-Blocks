// @davidberneda August 2021

{$SCOPEDENUMS ON}
unit Vidi_Game;

interface

uses
  Vidi_Graphics, Unit_Game;

type
  TBooleanArray=Array of Boolean;

  TField=record
  private
    procedure ClearRow(const ARow:Integer);
    procedure ClearColumn(const AColumn:Integer);
    function ClearFull(var ARows,AColumns:TBooleanArray):Integer;
    function Copy:TField;
    function EnoughFreeSpace:Boolean;
    function FreeCount:Integer;
    function FreePercentage:Single;
    procedure GetFull(var ARows,AColumns:TBooleanArray);
    procedure Paint(const Canvas:ICanvas; const Size:TPointFloat);
  public
    Columns:Integer;
    Rows:Integer;

    BlockColor,
    BlockBorderColor,
    ColorA,
    ColorB:TAlphaColor;

    Cells : Array of Array of Boolean;

    procedure Clear;

    function FullColumn(const AColumn:Integer):Boolean;
    function FullRow(const ARow:Integer):Boolean;

    procedure Init;
  end;

  TBits=class
  private
    function FitAt(const AField:TField; const P:TPoint):Boolean;
  public
  const
    MaxBlockSize=5;

  var
    Data:Array[0..MaxBlockSize-1,0..MaxBlockSize-1] of Boolean;

    Width,
    Height: Integer;

    procedure Clear;

    procedure SetField(const AtPos:TPoint; const Field:TField);
    function TrueCount:Integer;
  end;

  TBlock=(None,  // nothing

            Small0,   // |-
            Small1,   // -|
            Small2,   // |_
            Small3,   // _|

            Big0,     // L  3x3
            Big1,
            Big2,
            Big3,

            TwoByTwo, // 2x2
            Full,     // 3x3

            // horizontal
            One,      // -
            Two,      // --
            Three,    // ---
            Four,     // ----
            Five,     // -----

            // vertical
            VTwo,      // --
            VThree,    // ---
            VFour,     // ----
            VFive,     // -----

            Cross,   // +
            Tee0,    // T
            Tee1,    // |-
            Tee2,    // |_
            Tee3     // -|

            // Long L...
            );

  TDroppedAnimation=record
  public
    Block : TBlock;
    Position : TPoint;
    Value : Single;
  end;

  TCleanAnimation=record
  public
    Enabled : Boolean;

    Value : Single;

    Rows,
    Columns : TBooleanArray;
  end;

  TBlocks=class(TGame)
  private
  var
    Field : TField;

    Size : TPointFloat;

    WillKillGame,
    ForcedFit : Boolean;
    Fit : TPoint;

    Drag : TPointFloat;
    Dragged : Integer;

    CleanAnimation : TCleanAnimation;
    DroppedAnimation : TDroppedAnimation;

  const
    SmallBlocks:Array of TBlock=[TBlock.Small0,TBlock.Small1,
                                 TBlock.Small2,TBlock.Small3,
                                 TBlock.TwoByTwo,TBlock.One,
                                 TBlock.Two,TBlock.Three,
                                 TBlock.VTwo,TBlock.VThree];
  type
    TNewBlock=record
      Block : TBlock;
      Disabled: Boolean;
      Bounds : TRectFloat;

      function Bits:TBits; inline;
      function Contains(const P:TPointFloat):Boolean;
    end;

    function BlockAt(const P:TPointFloat):TPoint;
    function BlockUnder(const P:TPointFloat):Integer;
    function CalcDragPosition(const X, Y: Single):TPointFloat;
    function CalcFit:TPoint;
    procedure DrawBlock(const ABlock:TNewBlock; const AOrder:Integer);
    procedure GenerateBits;

    function NewBlocksValid:Integer;
    function OneBlockCanFit(const AField:TField; const ExceptNew:Integer):Boolean;
    procedure PaintBits(const Animation:TDroppedAnimation); overload;
    procedure PaintClean;

    function SearchOnlyPossibleFit(const ABits:TBits; var AFit:TPoint):Boolean;
    procedure SetNewBlocks;
  public

  const
    NewBlockCount=3;

  class var
    BitsOf:Array[TBlock] of TBits;

  var
    NewBlocks : Array[0..NewBlockCount-1] of TNewBlock;

    DisabledBlockColor,
    NewBlocksAreaColor : TAlphaColor;

    BigLimit : Integer;

    LockRotation : Boolean;

    Language : String;

    Moves : Integer;

    Constructor Create(const ACanvas:ICanvas); override;
    Destructor Destroy; override;

    function CalculateDisabled:Boolean;

    procedure MouseDown(const X,Y: Single);
    procedure MouseMove(const X,Y: Single);
    procedure MouseUp(const X,Y: Single);

    procedure NewGame;
    procedure Paint;
    function PaintFree(out Value:Single):TAlphaColor;
    procedure Resize(const ABounds:TRectFloat); override;

    procedure LoadMatch(const AProc:TIntegerFunction);
    procedure SaveMatch(const AProc:TIntegerProcedure);
  end;

implementation

uses
  Unit_Animate;

{ TField }

procedure TField.Clear;
var X,Y : Integer;
begin
  SetLength(Cells,Columns,Rows);

  for X:=0 to Columns-1 do
      for Y:=0 to Rows-1 do
          Cells[X,Y]:=False;
end;

procedure TField.Init;
begin
  Columns:=10;
  Rows:=10;

  BlockColor:=TAlphaColorRec.NavajoWhite;
  BlockBorderColor:=TAlphaColorRec.Brown;

  ColorA:=TAlphaColorRec.Sienna;
  ColorB:=TAlphaColorRec.Chocolate;
end;

function TField.FullColumn(const AColumn:Integer):Boolean;
var Y : Integer;
begin
  for Y:=0 to Rows-1 do
      if not Cells[AColumn,Y] then
         Exit(False);

  result:=True;
end;

function TField.FullRow(const ARow:Integer):Boolean;
var X : Integer;
begin
  for X:=0 to Columns-1 do
      if not Cells[X,ARow] then
         Exit(False);

  result:=True;
end;

procedure TField.ClearRow(const ARow:Integer);
var X : Integer;
begin
  for X:=0 to Columns-1 do
      Cells[X,ARow]:=False;
end;

procedure TField.ClearColumn(const AColumn:Integer);
var Y : Integer;
begin
  for Y:=0 to Rows-1 do
      Cells[AColumn,Y]:=False;
end;

function TField.FreeCount:Integer;
var X, Y : Integer;
begin
  result:=0;

  for X:=0 to Columns-1 do
      for Y:=0 to Rows-1 do
          if not Cells[X,Y] then
             Inc(result);
end;

function TField.FreePercentage:Single;
begin
  result:=FreeCount/(Columns*Rows);
end;

function TField.Copy:TField;
var X,Y : Integer;
begin
  result.Columns:=Columns;
  result.Rows:=Rows;

  SetLength(result.Cells,Columns,Rows);

  for X:=0 to Columns-1 do
      for Y:=0 to Rows-1 do
          result.Cells[X,Y]:=Cells[X,Y];
end;

function TField.EnoughFreeSpace:Boolean;
begin
  result:=FreePercentage>0.5;
end;

procedure TField.GetFull(var ARows,AColumns: TBooleanArray);
var X,Y :Integer;
begin
  // First remember full rows and columns
  SetLength(AColumns,Columns);
  for X:=0 to Columns-1 do
      AColumns[X]:=FullColumn(X);

  SetLength(ARows,Rows);
  for Y:=0 to Rows-1 do
      ARows[Y]:=FullRow(Y);
end;

// Returns extra-score
function TField.ClearFull(var ARows,AColumns:TBooleanArray): Integer;
var X,Y : Integer;
begin
  result:=0;

  // Now remove full rows and columns
  for X:=0 to Columns-1 do
      if AColumns[X] then
      begin
        Inc(result,Rows);
        ClearColumn(X);
      end;

  for Y:=0 to Rows-1 do
      if ARows[Y] then
      begin
        Inc(result,Columns);
        ClearRow(Y);
      end;
end;

procedure TField.Paint(const Canvas: ICanvas; const Size:TPointFloat);

  function ChessColor(const X,Y:Integer):TAlphaColor;
  begin
    if X mod 2=1 then
       if Y mod 2=1 then
          result:=ColorA
       else
          result:=ColorB
    else
       if Y mod 2=1 then
          result:=ColorB
       else
          result:=ColorA;
  end;

var X,Y : Integer;
    R : TRectFloat;
begin
  Canvas.FillKind(TGraphicKind.Solid);

  Canvas.StrokeKind(TGraphicKind.Solid);
  Canvas.StrokeColor(BlockBorderColor);

  for X:=0 to Columns-1 do
  begin
    R.Left:=X*Size.X;
    R.Right:=R.Left+Size.X;

    for Y:=0 to Rows-1 do
    begin
      R.Top:=Y*Size.Y;
      R.Bottom:=R.Top+Size.Y;

      if Cells[x,y] then
         Canvas.FillColor(BlockColor)
      else
         Canvas.FillColor(ChessColor(X,Y));

      Canvas.FillRect(R);

      if Cells[x,y] then
         Canvas.DrawRect(R);
    end;
  end;
end;

{ TBits }

procedure TBits.Clear;
var X,Y : Integer;
begin
  for X:=0 to MaxBlockSize-1 do
      for Y:=0 to MaxBlockSize-1 do
          Data[X,Y]:=False;

  Width:=0;
  Height:=0;
end;

function TBits.TrueCount:Integer;
var X, Y : Integer;
begin
  result:=0;

  for X:=0 to MaxBlockSize-1 do
      for Y:=0 to MaxBlockSize-1 do
          if Data[X,Y] then
             Inc(result);
end;

procedure TBits.SetField(const AtPos:TPoint; const Field:TField);
var X,Y : Integer;
begin
  for X:=0 to MaxBlockSize-1 do
      for Y:=0 to MaxBlockSize-1 do
          if Data[X,Y] then
             Field.Cells[AtPos.X+X,AtPos.Y+Y]:=True;
end;

function TBits.FitAt(const AField:TField; const P:TPoint):Boolean;
var  X, Y : Integer;
begin
  result:=False;

  if (P.X<0) or (P.Y<0) then
     Exit;

  for X:=0 to TBits.MaxBlockSize-1 do
  begin
    for Y:=0 to TBits.MaxBlockSize-1 do
    begin
      if Data[X,Y] then
      begin
        if P.X+X>=AField.Columns then
           Exit
        else
        if P.Y+Y>=AField.Rows then
           Exit
        else
        if AField.Cells[P.X+X,P.Y+Y] then
           Exit;
      end;
    end;
  end;

  result:=True;
end;

{ TNewBlock }

function TBlocks.TNewBlock.Bits:TBits;
begin
  result:=BitsOf[Block];
end;

function TBlocks.TNewBlock.Contains(const P:TPointFloat):Boolean;
begin
  result:=(Block<>TBlock.None) and Bounds.Contains(P);
end;

{ TBlocks }
procedure TBlocks.GenerateBits;

  function GetBitsOf(const ABlock:TBlock):TBits;

    procedure SetBit(X,Y:Integer);
    begin
      result.Data[X,Y]:=True;
    end;

    procedure SetSize(X,Y:Integer);
    begin
      result.Width:=X;
      result.Height:=Y;
    end;

  begin
    result:=TBits.Create;
    result.Clear;

    case ABlock of
    TBlock.Small0: begin SetBit(0,0); SetBit(0,1); SetBit(1,1); SetSize(2,2); end;
    TBlock.Small1: begin SetBit(0,1); SetBit(1,1); SetBit(1,0); SetSize(2,2); end;
    TBlock.Small2: begin SetBit(0,0); SetBit(0,1); SetBit(1,0); SetSize(2,2); end;
    TBlock.Small3: begin SetBit(0,0); SetBit(1,1); SetBit(1,0); SetSize(2,2); end;

      TBlock.Big0: begin SetBit(0,0); SetBit(0,1); SetBit(0,2); SetBit(1,2); SetBit(2,2); SetSize(3,3); end;
      TBlock.Big1: begin SetBit(2,0); SetBit(2,1); SetBit(2,2); SetBit(1,2); SetBit(0,2); SetSize(3,3); end;
      TBlock.Big2: begin SetBit(0,0); SetBit(0,1); SetBit(0,2); SetBit(1,0); SetBit(2,0); SetSize(3,3); end;
      TBlock.Big3: begin SetBit(2,0); SetBit(2,1); SetBit(2,2); SetBit(1,0); SetBit(0,0); SetSize(3,3); end;

  TBlock.TwoByTwo: begin
              SetBit(0,0); SetBit(1,0);
              SetBit(0,1); SetBit(1,1);
              SetSize(2,2);
            end;

      TBlock.Full: begin
              SetBit(0,0); SetBit(1,0); SetBit(2,0);
              SetBit(0,1); SetBit(1,1); SetBit(2,1);
              SetBit(0,2); SetBit(1,2); SetBit(2,2);
              SetSize(3,3);
            end;

       TBlock.One: begin SetBit(0,0); SetSize(1,1); end;

     TBlock.Two:   begin SetBit(0,0); SetBit(1,0); SetSize(2,1); end;
     TBlock.Three: begin SetBit(0,0); SetBit(1,0); SetBit(2,0); SetSize(3,1); end;
     TBlock.Four:  begin SetBit(0,0); SetBit(1,0); SetBit(2,0); SetBit(3,0); SetSize(4,1); end;
     TBlock.Five:  begin SetBit(0,0); SetBit(1,0); SetBit(2,0); SetBit(3,0); SetBit(4,0); SetSize(5,1); end;

    TBlock.VTwo:   begin SetBit(0,0); SetBit(0,1); SetSize(1,2); end;
    TBlock.VThree: begin SetBit(0,0); SetBit(0,1); SetBit(0,2); SetSize(1,3); end;
    TBlock.VFour:  begin SetBit(0,0); SetBit(0,1); SetBit(0,2); SetBit(0,3); SetSize(1,4); end;
    TBlock.VFive:  begin SetBit(0,0); SetBit(0,1); SetBit(0,2); SetBit(0,3); SetBit(0,4); SetSize(1,5); end;

    TBlock.Cross:  begin SetBit(0,1); SetBit(1,1); SetBit(2,1); SetBit(1,0); SetBit(1,2); SetSize(3,3); end;

    // Small "T"s
    TBlock.Tee0:   begin SetBit(0,0); SetBit(1,0); SetBit(2,0); SetBit(1,1); SetSize(3,2); end;
    TBlock.Tee1:   begin SetBit(0,0); SetBit(0,1); SetBit(0,2); SetBit(1,1); SetSize(2,3); end;
    TBlock.Tee2:   begin SetBit(1,0); SetBit(1,1); SetBit(1,2); SetBit(0,1); SetSize(2,3); end;
    TBlock.Tee3:   begin SetBit(0,1); SetBit(1,1); SetBit(2,1); SetBit(1,0); SetSize(3,2); end;
    end;
  end;

var t : TBlock;
begin
  for t:=Low(TBlock) to High(TBlock) do
      BitsOf[t]:=GetBitsOf(t);
end;

function Minimum(const A,B:Single):Single;
begin
  if A>B then
     result:=B
  else
     result:=A;
end;

procedure TBlocks.DrawBlock(const ABlock:TNewBlock; const AOrder:Integer);

  procedure DoDrawBlock(const APos:TPointFloat; const ASize:Single);

    procedure Rect(const XOffset,YOffset:Integer);
    var R : TRectFloat;
    begin
      R.Left:=APos.X+(XOffset*ASize);
      R.Right:=R.Left+ASize;

      R.Top:=APos.Y+(YOffset*ASize);
      R.Bottom:=R.Top+ASize;

      Canvas.FillRect(R);
      Canvas.DrawRect(R);
    end;

  var X, Y : Integer;
  begin
    for X:=0 to TBits.MaxBlockSize-1 do
        for Y:=0 to TBits.MaxBlockSize-1 do
            if BitsOf[ABlock.Block].Data[X,Y] then
               Rect(X,Y);
  end;

  // Center block in bounds rectangle
  function CenteredBlock(const R:TRectFloat; const ABlock:TBlock; const ASize:Single):TPointFloat;
  begin
    result.X:=R.Left+(R.Width-(BitsOf[ABlock].Width*ASize))*0.5;
    result.Y:=R.Top+(R.Height-(BitsOf[ABlock].Height*ASize))*0.5;
  end;

var BlockSize : Single;
begin
  Canvas.StrokeKind(TGraphicKind.Solid);
  Canvas.StrokeColor(TAlphaColorRec.Brown);

  Canvas.FillKind(TGraphicKind.Solid);

  if ABlock.Disabled then
     Canvas.FillColor(DisabledBlockColor)
  else
     Canvas.FillColor(Field.BlockColor);

  if Dragged=AOrder then
     DoDrawBlock(Drag,Size.X)
   else
  begin
     BlockSize:=Minimum(ABlock.Bounds.Width,ABlock.Bounds.Height)*0.2;

    DoDrawBlock(CenteredBlock(ABlock.Bounds,ABlock.Block,BlockSize),BlockSize);
  end;
end;

function TBlocks.BlockUnder(const P:TPointFloat):Integer;
var t : Integer;
begin
  for t:=0 to NewBlockCount-1 do
      if NewBlocks[t].Contains(P) then
            Exit(t);

  result:=-1;
end;

procedure TBlocks.MouseDown(const X, Y: Single);
begin
  Dragged:=BlockUnder(TPointFloat.Create(X,Y));

  if Dragged<>-1 then
  begin
    if NewBlocks[Dragged].Disabled then
       Dragged:=-1
    else
    begin
      Drag:=CalcDragPosition(X,Y);
      Invalidate(Self);
    end;
  end;
end;

function DarkColor(const AColor:TAlphaColor; const Amount:Byte):TAlphaColor;
begin
  TAlphaColorRec(Result).A := TAlphaColorRec(AColor).A;

  if TAlphaColorRec(AColor).R-Amount<0 then
     TAlphaColorRec(Result).R:=0
  else
     TAlphaColorRec(Result).R:=TAlphaColorRec(AColor).R-Amount;

  if TAlphaColorRec(AColor).G-Amount<0 then
     TAlphaColorRec(Result).G:=0
  else
     TAlphaColorRec(Result).G:=TAlphaColorRec(AColor).G-Amount;

  if TAlphaColorRec(AColor).B-Amount<0 then
     TAlphaColorRec(Result).B:=0
  else
     TAlphaColorRec(Result).B:=TAlphaColorRec(AColor).B-Amount;
end;

function InterpolateColor(const Start, Stop: TAlphaColor; const T: Single): TAlphaColor;
begin
  TAlphaColorRec(Result).A := TAlphaColorRec(Start).A + Trunc((TAlphaColorRec(Stop).A - TAlphaColorRec(Start).A) * T);
  TAlphaColorRec(Result).R := TAlphaColorRec(Start).R + Trunc((TAlphaColorRec(Stop).R - TAlphaColorRec(Start).R) * T);
  TAlphaColorRec(Result).G := TAlphaColorRec(Start).G + Trunc((TAlphaColorRec(Stop).G - TAlphaColorRec(Start).G) * T);
  TAlphaColorRec(Result).B := TAlphaColorRec(Start).B + Trunc((TAlphaColorRec(Stop).B - TAlphaColorRec(Start).B) * T);
end;

procedure TBlocks.PaintClean;
var X,Y : Integer;
    XMargin,
    YMargin : Single;
    R : TRectFloat;
begin
  if (CleanAnimation.Value=1) or (CleanAnimation.Value=0) then
     Exit;

  Canvas.FillKind(TGraphicKind.Solid);
  Canvas.FillColor(Field.BlockColor);

  XMargin:=Size.X*CleanAnimation.Value*0.5;
  YMargin:=Size.Y*CleanAnimation.Value*0.5;

  for X:=0 to Field.Columns-1 do
  begin
    R.Left:=(X*Size.X)+XMargin;
    R.Right:=R.Left+Size.X-2*XMargin;

    if CleanAnimation.Columns[X] then
       for Y:=0 to Field.Rows-1 do
       begin
         R.Top:=(Y*Size.Y)+YMargin;
         R.Bottom:=R.Top+Size.Y-2*YMargin;

         Canvas.FillRect(R,1-CleanAnimation.Value);
       end;
  end;

  for Y:=0 to Field.Rows-1 do
  begin
    R.Top:=(Y*Size.Y)+YMargin;
    R.Bottom:=R.Top+Size.Y-2*YMargin;

    if CleanAnimation.Rows[Y] then
       for X:=0 to Field.Columns-1 do
       begin
         R.Left:=(X*Size.X)+XMargin;
         R.Right:=R.Left+Size.X-2*XMargin;

         Canvas.FillRect(R,1-CleanAnimation.Value);
       end;
  end;
end;

procedure TBlocks.PaintBits(const Animation:TDroppedAnimation);

  function AnimationColor(const AColor:TAlphaColor):TAlphaColor;
  var Amount : Integer;
  begin
    Amount:=Trunc(64*Animation.Value);

    if (Amount=64) or (Amount=0) then
       result:=AColor
    else
    if Amount<32 then
       result:=DarkColor(AColor,2*Amount)
    else
       result:=DarkColor(AColor,64-(2*(Amount-32)));
  end;

var X, Y : Integer;
    R : TRectFloat;
begin
  Canvas.FillKind(TGraphicKind.Solid);
  Canvas.FillColor(AnimationColor(Field.BlockColor));

  for X:=0 to TBits.MaxBlockSize-1 do
  begin
    R.Left:=(Animation.Position.X+X)*Size.X;
    R.Right:=R.Left+Size.X;

    for Y:=0 to TBits.MaxBlockSize-1 do
        if BitsOf[Animation.Block].Data[X,Y] and
           Field.Cells[Animation.Position.X+X,Animation.Position.Y+Y] then
        begin
          R.Top:=(Animation.Position.Y+Y)*Size.Y;
          R.Bottom:=R.Top+Size.Y;

          Canvas.FillRect(R);
          Canvas.DrawRect(R);
        end;
  end;
end;

function TBlocks.SearchOnlyPossibleFit(const ABits:TBits; var AFit:TPoint):Boolean;
var Found : Boolean;
    X,Y : Integer;
    P : TPoint;
begin
  AFit.X:=-1;
  AFit.Y:=-1;

  Found:=False;

  for X:=0 to Field.Columns-1 do
  begin
    P.X:=X;

    for Y:=0 to Field.Rows-1 do
    begin
      P.Y:=Y;

      if ABits.FitAt(Field,P) then
      begin
        if Found then
           Exit(False);

        Found:=True;
        AFit:=P;
      end;
    end;
  end;

  result:=Found;
end;

function TBlocks.CalcDragPosition(const X, Y: Single):TPointFloat;

  function ExtraSpace:Single;
  begin
    {$IFDEF MSWINDOWS}
    result:=0;
    {$ELSE}
    result:=2; // 2 blocks to skip "finger" size
    {$ENDIF}
  end;

  function DraggedHeight:Single;
  begin
    result:=Size.Y*(NewBlocks[Dragged].Bits.Height+ExtraSpace);
  end;

  function DraggedWidth:Single;
  begin
    result:=Size.X*(NewBlocks[Dragged].Bits.Width+ExtraSpace);
  end;

begin
  if VerticalMode then
  begin
    result.X:=X;
    result.Y:=Y-DraggedHeight;
  end
  else
  begin
    result.X:=X-DraggedWidth;
    result.Y:=Y;
  end;
end;

function TBlocks.BlockAt(const P:TPointFloat):TPoint;
begin
  result.X:=Round(P.X/Size.X);
  result.Y:=Round(P.Y/Size.Y);
end;

function TBlocks.CalcFit:TPoint;
var P : TPoint;
begin
  P:=BlockAt(Drag);

  ForcedFit:=False;

  if NewBlocks[Dragged].Bits.FitAt(Field,P) then
     result:=P
  else
  if SearchOnlyPossibleFit(NewBlocks[Dragged].Bits,result) then
     ForcedFit:=True
  else
     result:=TPoint.Create(-1,-1);
end;

// Can at least one new block fit at AField?
function TBlocks.OneBlockCanFit(const AField:TField; const ExceptNew:Integer):Boolean;

  // Can a block fit at any field position?
  function BlockCanFit(const AIndex:Integer; const AField:TField):Boolean;
  var X, Y : Integer;
  begin
    for X:=0 to AField.Columns-1 do
        for Y:=0 to AField.Rows-1 do
            if NewBlocks[AIndex].Bits.FitAt(AField,TPoint.Create(X,Y)) then
               Exit(True);

    result:=False;
  end;
var t : Integer;
begin
  for t:=0 to NewBlockCount-1 do
      if (ExceptNew=-1) or (t<>ExceptNew) then
         if NewBlocks[t].Block<>TBlock.None then
            if BlockCanFit(t,AField) then
               Exit(True);

  result:=False;
end;

function TBlocks.NewBlocksValid:Integer;
var t : Integer;
begin
  result:=0;

  for t:=0 to NewBlockCount-1 do
      if NewBlocks[t].Block<>TBlock.None then
         Inc(result);
end;

procedure TBlocks.MouseMove(const X, Y: Single);

  procedure TryChangeCursor;
  var tmp : Integer;
  begin
    tmp:=BlockUnder(TPointFloat.Create(X,Y));

    if tmp=-1 then
       SetCursor(Self,TMouseCursor.Normal)
    else
    if NewBlocks[tmp].Disabled then
       SetCursor(Self,TMouseCursor.Disabled)
    else
       SetCursor(Self,TMouseCursor.Hand);
  end;

  procedure CalculateWillKill;
  var NewField : TField;
      ARows,
      AColumns : TBooleanArray;
  begin
    if Fit.X=-1 then
       WillKillGame:=False
    else
    begin
      WillKillGame:=NewBlocksValid>1;

      if WillKillGame then
      begin
        NewField:=Field.Copy;
        NewBlocks[Dragged].Bits.SetField(Fit,NewField);

        NewField.GetFull(ARows,AColumns);
        NewField.ClearFull(ARows,AColumns);

        WillKillGame:=not OneBlockCanFit(NewField,Dragged);
      end;
    end;
  end;

begin
  if Dragged=-1 then
     TryChangeCursor
  else
  begin
    //if (X>=0) and (X<=Width) then
    begin
      Drag:=CalcDragPosition(X,Y);
      Fit:=CalcFit;

      CalculateWillKill;

      Invalidate(Self);
    end;
  end;
end;

function TBlocks.CalculateDisabled:Boolean;
var t : Integer;
    P : TPoint;
    WillDisable : Boolean;
begin
  result:=False;

  for t:=0 to NewBlockCount-1 do
      if NewBlocks[t].Block=TBlock.None then
         NewBlocks[t].Disabled:=False
      else
      begin
        SearchOnlyPossibleFit(NewBlocks[t].Bits,P);

        WillDisable:=P.X=-1;

        // Should we play the disabled sound?
        if WillDisable and (not NewBlocks[t].Disabled) then
           result:=True;

        NewBlocks[t].Disabled:=WillDisable;
      end;
end;

procedure TBlocks.MouseUp;

  procedure AddScore(const Value:Integer);
  begin
    Inc(Score,Value);

    if Score>BestScore then
       BestScore:=Score;

    RefreshScore(Self);
  end;

  procedure DropFit;
  begin
    NewBlocks[Dragged].Bits.SetField(Fit,Field);

    DroppedAnimation.Block:=NewBlocks[Dragged].Block;
    DroppedAnimation.Position:=Fit;

    TAnimate.Linear(500,procedure(const Value:Single)
    begin
      DroppedAnimation.Value:=Value;

      if Value=1 then
         DroppedAnimation.Block:=TBlock.None;

      Invalidate(Self);
    end);

    AddScore(NewBlocks[Dragged].Bits.TrueCount);

    Inc(Moves);
  end;

  function CountTrue(const Items:TBooleanArray):Integer;
  var t : Integer;
  begin
    result:=0;

    for t:=Low(Items) to High(Items) do
        if Items[t] then
           Inc(result);
  end;

  procedure TryClean;
  var ExtraScore : Integer;

      ToRemove : Integer;
  begin
    Field.GetFull(CleanAnimation.Rows,CleanAnimation.Columns);

    ToRemove:=CountTrue(CleanAnimation.Rows)+CountTrue(CleanAnimation.Columns);

    CleanAnimation.Enabled:=ToRemove>0;

    if ToRemove>0 then
    begin
      if ToRemove=1 then
         PlaySound('tap1')
      else
      if ToRemove>1 then
         PlaySound('tap2');

      ExtraScore:=Field.ClearFull(CleanAnimation.Rows,CleanAnimation.Columns);

      // Extra score?
      if ExtraScore>0 then
         AddScore(ExtraScore);

      TAnimate.Linear(250,procedure(const Value:Single)
      begin
        CleanAnimation.Value:=Value;
        CleanAnimation.Enabled:=Value<1;

        Invalidate(Self);
      end);
    end;
  end;

const
  GameOverSound='metal';

var P : TPointFloat;
begin
  if (Fit.X<>-1) and ForcedFit then
  begin
    P:=CalcDragPosition(X,Y);

    if not NewBlocks[Dragged].Bits.FitAt(Field,BlockAt(P)) then
    begin
      Fit.X:=-1;
      Fit.Y:=-1;
    end;
  end;

  if Fit.X<>-1 then
  begin
    PlaySound('place');

    DropFit;

    NewBlocks[Dragged].Block:=TBlock.None;

    TryClean;

    Fit.X:=-1;
    Fit.Y:=-1;

    if NewBlocksValid=0 then
       SetNewBlocks;

    if CalculateDisabled then
       PlaySound('metal');

    if not OneBlockCanFit(Field,-1) then
    begin
      PlaySound(GameOverSound);
      Terminate(Self);
    end;
  end;

  Dragged:= -1;

  Invalidate(Self);
end;

{ TBlocks }

Constructor TBlocks.Create(const ACanvas:ICanvas);
begin
  inherited;

  Field.Init;

  DisabledBlockColor:=TAlphaColorRec.Darkgrey;
  NewBlocksAreaColor:=TAlphaColorRec.Create(25,50,25); // green

  GenerateBits;

  NewGame;
end;

Destructor TBlocks.Destroy;
var t : TBlock;
begin
  for t:=Low(TBlock) to High(TBlock) do
      BitsOf[t].Free;

  inherited;
end;

procedure TBlocks.Resize(const ABounds:TRectFloat);

  procedure CalculateVertical;
  var Left,Top : Single;

      Margin,
      BlockWidth,
      BlockHeight : Single;

      t : Integer;
  begin
    BlockWidth:=Width/3.4;
    BlockHeight:=(Height-Width)/1.4;

    if BlockHeight>BlockWidth then
       BlockHeight:=BlockWidth;

    Margin:=BlockWidth*0.1;

    Left:=Margin;
    Top:=Width+(Height-Width)*0.1;

    for t:=0 to NewBlockCount-1 do
    begin
      NewBlocks[t].Bounds:=TRectFloat.Create(Left,Top,Left+BlockWidth,Top+BlockHeight);
      Left:=Left+BlockWidth+Margin;
    end;
  end;

  procedure CalculateHorizontal;
  var Left,Top : Single;

      Margin,
      BlockWidth,
      BlockHeight : Single;

      t : Integer;
  begin
    BlockHeight:=Height/3.4;
    BlockWidth:=(Width-Height)/1.5;

    if BlockWidth>BlockHeight then
       BlockWidth:=BlockHeight;

    Margin:=BlockHeight*0.1;

    Left:=Height+(Width-Height)*0.1;
    Top:=Margin;

    for t:=0 to NewBlockCount-1 do
    begin
      NewBlocks[t].Bounds:=TRectFloat.Create(Left,Top,Left+BlockWidth,Top+BlockHeight);
      Top:=Top+BlockHeight+Margin;
    end;
  end;

begin
  inherited;

  if VerticalMode then
     Bounds.Bottom:=Bounds.Top+Width
  else
     Bounds.Right:=Bounds.Left+Height;

  Size.X:=Bounds.Width/Field.Columns;
  Size.Y:=Bounds.Height/Field.Rows;

  if VerticalMode then
     CalculateVertical
  else
     CalculateHorizontal;
end;

function RandomBlock:TBlock;
var Max : Integer;
begin
  Max:=Ord(High(TBlock));
  result:=TBlock(1+Random(Max));
end;

function RandomBlockSmall:TBlock;
var Max : Integer;
begin
  Max:=Length(TBlocks.SmallBlocks);
  result:=TBlocks.SmallBlocks[Random(Max)];
end;

procedure TBlocks.SetNewBlocks;

  function BestRandom:TBlock;
  begin
    if Random(10)>BigLimit then
       result:=RandomBlock
    else
       result:=RandomBlockSmall;
  end;

  procedure CalcRandomLimit;
  begin
    if (Score>=BestScore) or Field.EnoughFreeSpace then  // high score mode !
       BigLimit:=0
    else
    if BestScore>0 then
       BigLimit:=Trunc(10*(BestScore-Score)/BestScore)  // progressive
    else
       BigLimit:=10; // first time play !
  end;

  procedure SetNewRandom(const Index:Integer; const TrySmall:Boolean);
  var Best : TBlock;
      P : TPoint;
      Count : Integer;
  begin
    if TrySmall then
    begin
      Count:=0;

      repeat
        Best:=RandomBlockSmall;
        SearchOnlyPossibleFit(BitsOf[Best],P);

        Inc(Count);

      until (P.X<>-1) or (Count>10);
    end
    else
    begin
      Best:=BestRandom;
      SearchOnlyPossibleFit(BitsOf[Best],P);
    end;

    NewBlocks[Index].Block:=Best;
    NewBlocks[Index].Disabled:= P.X=-1;
  end;

  procedure SetNewBlocks;
  var t : Integer;
  begin
    for t:=0 to NewBlockCount-1 do
        SetNewRandom(t,False);
  end;

  function AllDisabled:Boolean;
  var t : Integer;
  begin
    result:=True;

    for t:=0 to NewBlockCount-1 do
        if not NewBlocks[t].Disabled then
           Exit(False);
  end;

begin
  CalcRandomLimit;
  SetNewBlocks;

  if AllDisabled then
     SetNewRandom(Random(NewBlockCount),True);
end;

procedure TBlocks.NewGame;
begin
  Field.Clear;

  SetNewBlocks;

  Fit.X:=-1;
  Fit.Y:=-1;

  Dragged:= -1;

  Score:=0;
  Moves:=0;
end;

procedure TBlocks.Paint;

  procedure FillBlocksArea;
  var R : TRectFloat;
  begin
    Canvas.FillKind(TGraphicKind.Solid);
    Canvas.FillColor(NewBlocksAreaColor);

    if Height>Width then
       R:=TRectFloat.Create(0,Bounds.Height,Width,Height)
    else
       R:=TRectFloat.Create(Bounds.Width,0,Width,Height);

    Canvas.FillRect(R);
  end;

  procedure DrawFit;

    procedure FitBlock(const ABlock:TBlock);

      procedure Rect(X,Y:Integer);
      var R : TRectFloat;
      begin
        R.Left:=(Fit.X+X)*Size.X;
        R.Right:=R.Left+Size.X;
        R.Top:=(Fit.Y+Y)*Size.Y;
        R.Bottom:=R.Top+Size.Y;

        Canvas.FillRect(R);
        Canvas.DrawRect(R);
      end;

    var X, Y : Integer;
    begin
      for X:=0 to TBits.MaxBlockSize-1 do
          for Y:=0 to TBits.MaxBlockSize-1 do
              if BitsOf[ABlock].Data[X,Y] then
                 Rect(X,Y);
    end;

  begin
    Canvas.StrokeKind(TGraphicKind.Solid);

    Canvas.FillKind(TGraphicKind.Solid);

    if WillKillGame then
    begin
      if ForcedFit then
         Canvas.StrokeColor(TAlphaColorRec.Darkred)
      else
         Canvas.StrokeColor(TAlphaColorRec.Whitesmoke);

      Canvas.FillColor(TAlphaColorRec.LightBlack);
    end
    else
    if ForcedFit then
    begin
      Canvas.StrokeColor(TAlphaColorRec.Darkred);
      Canvas.FillColor(TAlphaColorRec.NiceRed);
    end
    else
    begin
      Canvas.StrokeColor(TAlphaColorRec.Limegreen);
      Canvas.FillColor(TAlphaColorRec.NiceGreen);
    end;

    FitBlock(NewBlocks[Dragged].Block);
  end;

  procedure PaintNewBlocks;
  var t : Integer;
  begin
    // First paint non-dragged blocks
    for t:=0 to NewBlockCount-1 do
        if t<>Dragged then
           if NewBlocks[t].Block<>TBlock.None then
              DrawBlock(NewBlocks[t],t);

    // Now paint dragged block on top of others
    if Dragged<>-1 then
       DrawBlock(NewBlocks[Dragged],Dragged);
  end;

begin
  VerticalMode:=Height>Width;

  Field.Paint(Canvas,Size);

  if CleanAnimation.Enabled then
     PaintClean;

  if DroppedAnimation.Block<>TBlock.None then
     PaintBits(DroppedAnimation);

  if Fit.X<>-1 then
     DrawFit;

  FillBlocksArea;
  PaintNewBlocks;
end;

// Paint indicator of how many field cells are available (free)
function TBlocks.PaintFree(out Value:Single):TAlphaColor;
begin
  Value:=Field.FreePercentage;

  if Value>0.75 then
     result:=InterpolateColor(TAlphaColorRec.GreenYellow,TAlphaColorRec.NiceGreen,4*(Value-0.75))
  else
  if Value>0.5 then
     result:=InterpolateColor(TAlphaColorRec.Orange,TAlphaColorRec.GreenYellow,4*(Value-0.5))
  else
  if Value>0.25 then
     result:=InterpolateColor(TAlphaColorRec.NiceRed,TAlphaColorRec.Orange,4*(Value-0.25))
  else
     result:=TAlphaColorRec.NiceRed;
end;

procedure TBlocks.LoadMatch(const AProc:TIntegerFunction);
var X,Y, t : Integer;
begin
  Score:=AProc;
  Field.Columns:=AProc;
  Field.Rows:=AProc;

  for X:=0 to Field.Columns-1 do
      for Y:=0 to Field.Rows-1 do
          Field.Cells[X,Y]:=AProc=1;

//  NewBlockCount:=AProc; <-- read-only

  for t:=0 to NewBlockCount-1 do
      NewBlocks[t].Block:=TBlock(AProc);
end;

procedure TBlocks.SaveMatch(const AProc:TIntegerProcedure);
var X,Y, t : Integer;
begin
  AProc(Score);
  AProc(Field.Columns);
  AProc(Field.Rows);

  for X:=0 to Field.Columns-1 do
      for Y:=0 to Field.Rows-1 do
          AProc(Ord(Field.Cells[X,Y]));

//  AProc(NewBlockCount);

  for t:=0 to NewBlockCount-1 do
      AProc(Ord(NewBlocks[t].Block));
end;

end.
