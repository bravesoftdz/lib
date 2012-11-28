unit simple_parser_lib;

interface

uses SysUtils;

type

TSimpleParser=class
  private
    _str: string;
    _pos: Integer;
    procedure skip_spaces;
    procedure find_next_space;
    function isSpace(ch: char): boolean;
  public
    constructor Create; overload;
    constructor Create(str: String); overload;
    procedure AssignString(str: String);
    function eof: boolean;
    function getChar: char;
    function getFloat: Real;
  end;

implementation

constructor TSimpleParser.Create;
begin
  inherited Create;
  _pos:=1;
end;

constructor TSimpleParser.Create(str: String);
begin
  inherited Create;
  _str:=str;
  _pos:=1;
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
begin
  result:= (ch=' ') or (ch=#9) or (ch=#10) or (ch=#13);
end;

procedure TSimpleParser.skip_spaces;
begin
  while (_pos<=Length(_str)) and isSpace(_str[_pos]) do inc(_pos);
end;

procedure TSimpleParser.find_next_space;
begin
  while (_pos<=Length(_str)) and (not isSpace(_str[_pos])) do inc(_pos);
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
var i: Integer;
    ch: char;
    s: string;
begin
  skip_spaces;
  while _pos<=Length(_str) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'+') and (ch<>'-') and (ch<>'E') and (ch<>'e') and (ch<>'.') and (ch<>',') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;

  result:=StrToFloat(s);
end;

end.
