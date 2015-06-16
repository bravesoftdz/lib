unit specchars;

interface

type

wchobj=class
  public
    ch: widechar;
    constructor Create(ach: widechar); overload;
    constructor Create(i: Integer); overload;
  end;

TIntegerArray = array of Integer;

function ConvertSpecChars(text: string): string;
function ConvertSpecCharsMap(text: string; var map: TIntegerArray): string;

implementation
uses classes,strUtils,simple_parser_lib;

var idents: TStrings;
    maxidentlen: Integer;

function ConvertSpecCharsMap(text: string; var map: TIntegerArray): string;
var i,z,k,j: Integer;
    id: string;
begin
  result:=text;
  i:=1;
  while i<=Length(result) do begin
    if result[i]='\' then begin
      z:=1;
      id:='';
      k:=-1;
      while (k=-1) and (z<=maxidentlen) and (i+z <= Length(result)) do begin
        id:=id+result[i+z];
        k:=idents.IndexOf(id);
        inc(z);
      end;
      if k>=0 then begin
        result:=StuffString(result,i,z,wchobj(idents.Objects[k]).ch);
        for j:=i to Length(result)+1 do
          map[j]:=map[j+z-1];
        SetLength(map,Length(result)+2);
      end;
    end;
    inc(i);
  end;
end;

function ConvertSpecChars(text: string): string;
var i,z,k: Integer;
    id: string;
begin
  result:=text;
  i:=1;
  while i<=Length(result) do begin
    if result[i]='\' then begin
      z:=1;
      id:='';
      k:=-1;
      while (k=-1) and (z<=maxidentlen) and (i+z <= Length(result)) do begin
        id:=id+result[i+z];
        k:=idents.IndexOf(id);
        inc(z);
      end;
      if k>=0 then begin
        result:=StuffString(result,i,z+1,wchobj(idents.Objects[k]).ch);
      end;
    end;
    inc(i);
  end;
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
//  idents.AddObject('alpha',wchobj.Create($03b1));

  maxidentlen:=3;
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
