unit simple_parser_lib;

interface

uses SysUtils;

type

TSimpleParser=class
  private
    _str: string;
    _pos: Integer;
    procedure skip_spaces;
//    procedure find_next_space;
    function isSpace(ch: char): boolean;
    function isDelimiter(ch: char): boolean;
  public
    spaces: string;
    delimiter: string;
    constructor Create; overload;
    constructor Create(str: String); overload;
    procedure AssignString(str: String);
    function eof: boolean;
    function getChar: char;
    function NextChar: char;
    function getFloat: Real;
    function getInt: Integer;
    function getInt64: Int64;
    function getString: string;   //считать остаток строки
    function getHex: Integer;

  end;

implementation
uses strUtils;

constructor TSimpleParser.Create;
begin
  inherited Create;
  spaces:=' '+#9+#10+#13;
  delimiter:=';';
  _pos:=1;
end;

constructor TSimpleParser.Create(str: String);
begin
  Create;
  _str:=str;
end;

procedure TSimpleParser.AssignString(str: String);
begin
  _str:=str;
  _pos:=1;
end;

function TSimpleParser.eof: Boolean;
begin
  skip_spaces;
  result:=(_pos>Length(_str));
end;

function TSimpleParser.isSpace(ch: Char): Boolean;
var i: Integer;
begin
  for i:=1 to Length(spaces) do
    if ch=spaces[i] then begin
      result:=true;
      exit;
    end;
  result:=false;
//  result:= (ch=' ') or (ch=#9) or (ch=#10) or (ch=#13);
end;

function TSimpleParser.isDelimiter(ch: Char): Boolean;
var i: Integer;
begin
  for i:=1 to Length(delimiter) do
    if ch=delimiter[i] then begin
      result:=true;
      exit;
    end;
  result:=false;
end;


procedure TSimpleParser.skip_spaces;
begin
  while (_pos<=Length(_str)) and (isSpace(_str[_pos]) or isDelimiter(_str[_pos])) do inc(_pos);
end;
(*
procedure TSimpleParser.find_next_space;
begin
  while (_pos<=Length(_str)) and (not isSpace(_str[_pos])) do inc(_pos);
end;
*)
function TSimpleParser.NextChar: char;
begin
  skip_spaces;
  if _pos<=Length(_str) then result:=_str[_pos] else Raise Exception.Create('TParser.NextChar: unexpected end of string');
end;

function TSimpleParser.getChar: char;
var i: Integer;
begin
  skip_spaces;
  i:=_pos;
  inc(_pos);
  if i<=Length(_str) then result:=_str[i] else Raise Exception.Create('TParser.getChar: unexpected end of string');
end;

function TSimpleParser.getFloat: Real;
var ch: char;
    s: string;
    separator: char;
begin
  separator:=DecimalSeparator;
  skip_spaces;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'+') and (ch<>'-') and (ch<>'E') and (ch<>'e') and (ch<>'.') and (ch<>',') and (not isSpace(ch)) then break;
    if (ch=',') or (ch='.') then ch:=separator;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;

  result:=StrToFloat(s);
end;

function TSimpleParser.getInt: Integer;
var ch: char;
    s: string;
begin
  skip_spaces;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'-') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;
  if s='' then result:=0 else result:=StrToInt(s);
end;

function TSimpleParser.getInt64: Int64;
var ch: char;
    s: string;
begin
  skip_spaces;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'-') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;
  if s='' then result:=0 else result:=StrToInt64(s);
end;

function TSimpleParser.getString: string;
var i: Integer;
begin
//  Result:=RightStr(_str,Length(_str)-_pos+1);
  skip_spaces;
  i:=_pos;
  while (_pos<=Length(_str)) and (not isDelimiter(_str[_pos])) do inc(_pos);
  Result:=MidStr(_str,i,_pos-i);
end;

function TSimpleParser.getHex: Integer;
var x,Int0: Integer;
    ch: Integer;
begin
  int0:=Integer('0');
  x:=0;
  skip_spaces;
  while (_pos<=Length(_str)) and (_str[_pos]<='F') and (_str[_pos]>='0') do begin
    ch:=Integer(_str[_pos])-Int0;
    if ch>9 then ch:=ch-7;
    x:=(x shl 4) or ch;
    inc(_pos);
  end;
  Result:=x;
end;

end.
