unit specchars;

interface

type

wchobj=class
  public
    ch: widechar;
    constructor Create(ach: widechar); overload;
    constructor Create(i: Integer); overload;
  end;

function ConvertSpecChars(text: string): string;

implementation
uses classes,strUtils,simple_parser_lib;

var idents: TStrings;

function ConvertSpecChars(text: string): string;
var i,j,k: Integer;
    p: TSimpleParser;
    id: string;
begin
  result:='';
  j:=1;
  for i:=1 to Length(text) do begin
    if text[i]='\' then begin
      result:=result+MidStr(text,j,i-j);
      p:=TSimpleParser.Create(RightStr(text,Length(text)-i));
      id:=p.getIdent;
      k:=idents.IndexOf(id);
      if k=-1 then
        j:=i
      else begin
        result:=result+wchobj(idents.Objects[k]).ch;
        j:=i+Length(id);
      end;
    end;
  end;
  result:=result+RightStr(text,Length(text)-j);
end;

constructor wchobj.Create(ach: widechar);
begin
  ch:=ach;
end;

constructor wchobj.Create(i: Integer);
begin
  ch:=WideChar(i);
end;

procedure InitIdents;
begin
  idents:=TStringList.Create;
//  idents.AddObject('deg',wchobj.Create('°'));
  idents.AddObject('deg',wchobj.Create($00b0));
  idents.AddObject('alpha',wchobj.Create($03b1));


end;

procedure FreeIdents;
var i: Integer;
begin
  for i:=0 to idents.Count-1 do
    idents.Objects[i].Free;
  idents.Free;
end;

initialization
  InitIdents;
finalization
  FreeIdents;
end.
