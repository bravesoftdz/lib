unit HistoryFrame;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TFrameHistory = class(TFrame)
  private
    fSplitter: TSplitter;
    procedure SetSplitter(value: TSplitter);
    { Private declarations }
  protected
    procedure Notification(aComponent: TComponent; operation: TOperation); override;
  public
    { Public declarations }
    procedure AddLine(Xs,Xe,Y: Integer);
    procedure ClearLines;
    procedure HideWithSplitter;
    procedure ShowWithSplitter;
  published
    PaintBox1: TPaintBox;
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormPaint(Sender: TObject);
    property Splitter: TSplitter read fSplitter write SetSplitter;
  end;

  TLineRec=record
    Xs,Xe,Y: Integer;
  end;

var
  Lines: array of TLineRec;
implementation

{$R *.dfm}

procedure TFrameHistory.AddLine(Xs,Xe,Y: Integer);
var l: Integer;
begin
  l:=Length(Lines);
  SetLength(Lines,l+1);
  Lines[l].Xs:=Xs;
  Lines[l].Xe:=Xe;
  Lines[l].Y:=Y;
end;

procedure TFrameHistory.ClearLines;
begin
  SetLength(Lines,0);
end;

procedure TFrameHistory.FormPaint(Sender: TObject);
var i,x,y: Integer;
begin
  y:=self.VertScrollBar.Position;
  x:=self.HorzScrollBar.Position;
  with Paintbox1.Canvas do begin
    FillRect(self.BoundsRect);
    pen.Width:=2;
    pen.Color:=clBlack;
    pen.Style:=psSolid;
    for i:=0 to Length(Lines)-1 do begin
      MoveTo(Lines[i].Xs-x,Lines[i].Y-y);
      LineTo(Lines[i].Xe-x,Lines[i].Y-y);
      LineTo(Lines[i].Xe-5-x,Lines[i].Y-5-y);
      MoveTo(Lines[i].Xe-x,Lines[i].Y-y);
      LineTo(Lines[i].Xe-5-x,Lines[i].Y+5-y);
    end;
  end;
end;

procedure TFrameHistory.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Shift=[] then with VertScrollBar do
    Position:=Position-increment
  else if Shift=[ssShift] then with HorzScrollBar do
    Position:=Position-increment
end;

procedure TFrameHistory.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Shift=[] then with VertScrollBar do
    Position:=Position+increment
  else if Shift=[ssShift] then with HorzScrollBar do
    Position:=Position+increment
end;

procedure TFrameHistory.SetSplitter(value: TSplitter);
begin
  if Assigned(fSplitter) then
    fSplitter.RemoveFreeNotification(self);
  fSplitter:=value;
  if Assigned(fSplitter) then
    fSplitter.FreeNotification(self);
end;

procedure TFrameHistory.Notification(aComponent: TComponent; operation: TOperation);
begin
  inherited;
  if (aComponent=fSplitter) and (operation=opRemove) then
    fSplitter:=nil;
end;

procedure TFrameHistory.HideWithSplitter;
begin
  if Assigned(splitter) then splitter.Visible:=false;
  visible:=false;
end;

procedure TFrameHistory.ShowWithSplitter;
begin
  Left:=0;
  Visible:=true;
  if Assigned(splitter) then begin
    splitter.Left:=Left-1;
    splitter.Visible:=true;
  end;
end;

end.