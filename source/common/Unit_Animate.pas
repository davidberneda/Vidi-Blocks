// @davidberneda August 2021
unit Unit_Animate;

interface

type
  TAnimateProc=reference to procedure(const Value:Single);

  TAnimationStyle=(Linear);

  TAnimation=record
  private
    Started : Int64;
  public
    Duration : Integer;
    Proc : TAnimateProc;
    Style : TAnimationStyle;
  end;

  TAnimate=class
  private
    class var
       Items : Array of TAnimation;

    class procedure Add(const AItem:TAnimation); static;
  public
    class var
      OnEnable : procedure(const Value:Boolean) of object;
      GetTick : function:Cardinal of object;

    class procedure Run(const Mseconds:Integer; const AStyle:TAnimationStyle; const AProc:TAnimateProc); static;
    class procedure Linear(const Mseconds:Integer; const AProc:TAnimateProc); static;
    class procedure TimerEvent(Sender:TObject);
  end;

implementation

{ TAnimate }

class procedure TAnimate.Add(const AItem:TAnimation);
var L : Integer;
begin
  L:=Length(Items);
  SetLength(Items,L+1);
  Items[L]:=AItem;
end;

class procedure TAnimate.Run(const Mseconds: Integer;
  const AStyle: TAnimationStyle; const AProc: TAnimateProc);
var Item : TAnimation;
begin
  Item.Duration:=Mseconds;

  if Item.Duration<=0 then
     Item.Duration:=1;

  Item.Proc:=AProc;
  Item.Style:=AStyle;
  Item.Started:=GetTick;

  Add(Item);

  OnEnable(True);
end;

class procedure TAnimate.Linear(const Mseconds: Integer;
  const AProc: TAnimateProc);
begin
  Run(MSeconds,TAnimationStyle.Linear,AProc);
end;

class procedure TAnimate.TimerEvent(Sender: TObject);
var t: Integer;
    tmp : Integer;
begin
  t:=0;

  while t<Length(Items) do
  begin
    tmp:=GetTick-Items[t].Started;

    if tmp>Items[t].Duration then
    begin
      Items[t].Proc(1);
      Delete(Items,t,1);
    end
    else
    begin
      Items[t].Proc(tmp/Items[t].Duration);

      Inc(t);
    end;
  end;

  if Items=nil then
     OnEnable(False);
end;

end.
