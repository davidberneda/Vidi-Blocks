unit Vidi_Canvas;

interface

uses
  Vidi_Graphics, FMX.Graphics;

type
  TVidiCanvas=class(TInterfacedObject,ICanvas)
  private
    FCanvas : TCanvas;
  public
    procedure Draw(const B:TGameBitmap; const Dest:TRectFloat); overload;
    procedure Draw(const B:TGameBitmap; const Source,Dest:TRectFloat); overload;
    procedure DrawRect(const R:TRectFloat);
    procedure FillColor(const AColor:TAlphaColor);
    procedure FillKind(const AKind:TGraphicKind);
    procedure FillRect(const R:TRectFloat); overload;
    procedure FillRect(const R:TRectFloat; const Opacity:Single); overload;
    procedure StrokeColor(const AColor:TAlphaColor);
    procedure StrokeKind(const AKind:TGraphicKind);

    property Canvas:TCanvas read FCanvas write FCanvas;
  end;

implementation

uses
  System.Types;

{ TVidiCanvas }

procedure TVidiCanvas.Draw(const B: TGameBitmap; const Dest: TRectFloat);
begin
  FCanvas.DrawBitmap(TBitmap(B),TBitmap(B).BoundsF,TRectF(Dest),1,True);
end;

procedure TVidiCanvas.Draw(const B: TGameBitmap; const Source,Dest: TRectFloat);
begin
  FCanvas.DrawBitmap(TBitmap(B),TRectF(Source),TRectF(Dest),1,True);
end;

procedure TVidiCanvas.DrawRect(const R:TRectFloat);
begin
  FCanvas.DrawRect(TRectF(R),0,0,[],1);
end;

procedure TVidiCanvas.FillColor(const AColor:TAlphaColor);
begin
  FCanvas.Fill.Color:=AColor;
end;

procedure TVidiCanvas.FillKind(const AKind:TGraphicKind);
begin
  if AKind=TGraphicKind.None then
     FCanvas.Fill.Kind:=TBrushKind.None
  else
     FCanvas.Fill.Kind:=TBrushKind.Solid;
end;

procedure TVidiCanvas.FillRect(const R: TRectFloat; const Opacity: Single);
begin
  FCanvas.FillRect(TRectF(R),Opacity);
end;

procedure TVidiCanvas.FillRect(const R:TRectFloat);
begin
  FCanvas.FillRect(TRectF(R),1);
end;

procedure TVidiCanvas.StrokeColor(const AColor:TAlphaColor);
begin
  FCanvas.Stroke.Color:=AColor;
end;

procedure TVidiCanvas.StrokeKind(const AKind:TGraphicKind);
begin
  if AKind=TGraphicKind.None then
     FCanvas.Stroke.Kind:=TBrushKind.None
  else
     FCanvas.Stroke.Kind:=TBrushKind.Solid;
end;

end.
