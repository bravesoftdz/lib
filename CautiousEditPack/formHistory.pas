unit FormHistory;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TfrmHistory = class(TForm)
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
  published
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddLine(Xs,Xe,Y: Integer);
    procedure ClearLines;
  end;

  TLineRec=record
    Xs,Xe,Y: Integer;
  end;

var
  frmHistory: TfrmHistory;
  Lines: array of TLineRec;
implementation

{$R *.dfm}

procedure TfrmHistory.AddLine(Xs,Xe,Y: Integer);
var l: Integer;
begin
  l:=Length(Lines);
  SetLength(Lines,l+1);
  Lines[l].Xs:=Xs;
  Lines[l].Xe:=Xe;
  Lines[l].Y:=Y;
end;

procedure TfrmHistory.ClearLines;
begin
  SetLength(Lines,0);
end;

procedure TfrmHistory.FormPaint(Sender: TObject);
var i,x,y: Integer;
begin
  y:=self.VertScrollBar.Position;
  x:=self.HorzScrollBar.Position;
  with Canvas do begin
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

procedure TfrmHistory.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Shift=[] then with VertScrollBar do
    Position:=Position-increment
  else if Shift=[ssShift] then with HorzScrollBar do
    Position:=Position-increment
end;

procedure TfrmHistory.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if Shift=[] then with VertScrollBar do
    Position:=Position+increment
  else if Shift=[ssShift] then with HorzScrollBar do
    Position:=Position+increment
end;

end.
