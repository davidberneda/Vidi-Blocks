{$SCOPEDENUMS ON}
unit Vidi_Graphics;

interface

type
  TPoint=record
  public
    X,Y : Integer;

    class function Create(const AX,AY:Integer):TPoint; static; inline;
  end;

  TPointFloat=record
  public
    X,Y : Single;

    class function Create(const AX,AY:Single):TPointFloat; static; inline;
  end;

  TRectFloat=record
  public
    Left,Top,Right,Bottom : Single;

    class function Create(const ALeft,ATop,ARight,ABottom:Single):TRectFloat; static; inline;

    function Contains(const P:TPointFloat):Boolean;
    function Height:Single; inline;
    function Width:Single; inline;

    procedure Move(const DeltaX,DeltaY:Single);
  end;

  TAlphaColor=type Cardinal;

  TAlphaColorRec=record
  public
    R,G,B,A : Byte;

    const
      Brown         =TAlphaColor($FFA52A2A);
      Chocolate     =TAlphaColor($FFD2691E);
      DarkGrey      =TAlphaColor($FFA9A9A9);
      DarkRed       =TAlphaColor($FF8B0000);
      GreenYellow   =TAlphaColor($FFADFF2F);
      LightBlack    =TAlphaColor($FF2F2F2F);
      LimeGreen     =TAlphaColor($FF32CD32);
      NavajoWhite   =TAlphaColor($FFFFDEAD);
      NiceGreen     =TAlphaColor($FF008000);
      NiceRed       =TAlphaColor($FFDF0000);
      Orange        =TAlphaColor($FFFFA500);
      OrangeRed     =TAlphaColor($FFFF4500);
      Sienna        =TAlphaColor($FFA0522D);
      White         =TAlphaColor($FFFFFFFF);
      WhiteSmoke    =TAlphaColor($FFF5F5F5);

    class function Create(const Red,Green,Blue:Byte):TAlphaColor; static; inline;
  end;

  TGameBitmap=TObject;

  TGraphicKind=(None,Solid);

  ICanvas=Interface
    procedure Draw(const B:TGameBitmap; const Dest:TRectFloat); overload;
    procedure Draw(const B:TGameBitmap; const Source,Dest:TRectFloat); overload;
    procedure DrawRect(const R:TRectFloat);
    procedure FillColor(const AColor:TAlphaColor);
    procedure FillKind(const AKind:TGraphicKind);
    procedure FillRect(const R:TRectFloat); overload;
    procedure FillRect(const R:TRectFloat; const Opacity:Single); overload;
    procedure StrokeColor(const AColor:TAlphaColor);
    procedure StrokeKind(const AKind:TGraphicKind);
  end;

implementation

{ TPoint }

class function TPoint.Create(const AX,AY:Integer):TPoint;
begin
  result.X:=AX;
  result.Y:=AY;
end;

{ TPointFloat }

class function TPointFloat.Create(const AX,AY:Single):TPointFloat;
begin
  result.X:=AX;
  result.Y:=AY;
end;

{ TRectFloat }

class function TRectFloat.Create(const ALeft,ATop,ARight,ABottom:Single):TRectFloat;
begin
  result.Left:=ALeft;
  result.Top:=ATop;
  result.Right:=ARight;
  result.Bottom:=ABottom;
end;

function TRectFloat.Contains(const P: TPointFloat): Boolean;
begin
  result:=(P.X >= Left) and (P.X < Right) and (P.Y >= Top) and (P.Y < Bottom);
end;

function TRectFloat.Height:Single;
begin
  result:=Bottom-Top;
end;

procedure TRectFloat.Move(const DeltaX, DeltaY: Single);
begin
  Left:=Left+DeltaX;
  Right:=Right+DeltaX;
  Top:=Top+DeltaY;
  Bottom:=Bottom+DeltaY;
end;

function TRectFloat.Width:Single;
begin
  result:=Right-Left;
end;

{ TAlphaColorRec }

class function TAlphaColorRec.Create(const Red,Green,Blue:Byte):TAlphaColor;
begin
  TAlphaColorRec(Result).R := Red;
  TAlphaColorRec(Result).G := Green;
  TAlphaColorRec(Result).B := Blue;
  TAlphaColorRec(Result).A := $FF;
end;

end.
