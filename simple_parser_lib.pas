unit simple_parser_lib;

interface

uses SysUtils,ConvUtils;

type

TSimpleParser=class
  private
    function isSpace(ch: char): boolean;
    function isDelimiter(ch: char): boolean;
    function isPhysIdentSymbol(ch: char): boolean;
  protected
    _str: string;
    _pos: Integer;
    fBackupPos: Integer;
    procedure skip_spaces;
  public
    spaces: string;
    delimiter: string;
    class function isNumberSymbol(ch: char): boolean;
    class function isFirstIdentSymbol(ch: char): boolean;
    class function isIdentSymbol(ch: char): boolean;
    constructor Create; overload;
    constructor Create(str: String); overload;
    procedure AssignString(str: String);
    procedure PutBack;  //��� �� � ������� ��� ����� - "������"
    function eof: boolean;
    function getChar: char;
    function NextChar: char;
    function getFloat: Real;
    function getInt: Integer;
    function getInt64: Int64;
    function getString: string;   //������� ������� ������
    function getIdent: string; //������� �������, ���� ��� ������������ � ����. A..Z, a..z, 0..9, _, �..�, �..�
    function getPhysUnitIdent: string;
    function getVarPathIdent: string;
    function getHex(digits: Integer=-1): Integer;
    function getBinary: LongWord;
    property Pos: Integer read _pos;
  end;

EParserError=class(Exception);

resourcestring
  UnexpectedEndOfString = '����������� ����� ������: %s';
  UnknownSymbolAfterExponent = '�������� ������ ����� "E" (����������� �������� � ���������� �������) � %s';
  DigitExpected = '��������� ����� � %s';
  MinutesExpected = '�������� ������ '' (������)';
  SecondsExpected = '�������� ������ " (�������)';
  DigitExpectedAfterDecimalSeparator = '����� ������� ������ ���� ����� � %s';
  DigitExpectedAfterExponent = '����� ������� E ������ ���� ����� ����� � %s';

implementation
uses strUtils;

constructor TSimpleParser.Create;
begin
  inherited Create;
  spaces:=' '+#9+#10+#13;
  delimiter:=';';
  _pos:=1;
  fbackupPos:=-1; //������ ����������
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
var buPos: Integer;
begin
  buPos:=_pos;
  skip_spaces;
  result:=(_pos>Length(_str));
  _pos:=buPos;
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

class function TSimpleParser.isFirstIdentSymbol(ch: Char): Boolean;
begin
  result:=((ch>='A') and (ch<='Z')) or ((ch>='a') and (ch<='z')) or
    ((ch>='�') and (ch<='�')) or ((ch>='�') and (ch<='�')) or (ch='_');
end;

function TSimpleParser.isPhysIdentSymbol(ch: Char): Boolean;
begin
//����� �������, ���� ������: �����, ������ +-*/, ������
  result:=(ch<>'+') and (ch<>'-') and (ch<>'*') and (ch<>'/') and (ch<>'(') and
    (ch<>')') and not isSpace(ch) and (ch<>'[') and (ch<>']') and (ch<>'^');
end;

class function TSimpleParser.isNumberSymbol(ch: Char): Boolean;
begin
  result:=(ch>='0') and (ch<='9');
end;

class function TSimpleParser.isIdentSymbol(ch: Char): Boolean;
begin
  result:=IsFirstIdentSymbol(ch) or isNumberSymbol(ch);
end;



procedure TSimpleParser.PutBack;
begin
  if fBackupPos=-1 then raise EParserError.CreateFmt('Putback at %s: nothing to put back',[GetString]);
  _pos:=fBackupPos;
  fBackupPos:=-1;
end;

procedure TSimpleParser.skip_spaces;
begin
  while (_pos<=Length(_str)) and isSpace(_str[_pos]) do inc(_pos);
//  while (_pos<=Length(_str)) and (isSpace(_str[_pos]) or isDelimiter(_str[_pos])) do inc(_pos);
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
  if _pos<=Length(_str) then result:=_str[_pos] else Raise EParserError.Create('TParser.NextChar: unexpected end of string');
end;

function TSimpleParser.getChar: char;
begin
  skip_spaces;
  fBackupPos:=_pos;
  inc(_pos);
  if fBackupPos<=Length(_str) then result:=_str[fBackupPos] else Raise EParserError.Create('TParser.getChar: unexpected end of string');
end;

function TSimpleParser.getFloat: Real;
var ch: char;
    s: string;
    separator: char;
begin
  separator:=DecimalSeparator;
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'+') and (ch<>'-') and (ch<>'E') and (ch<>'e') and (ch<>'.') and (ch<>',') and (not isSpace(ch)) then break;
    if (ch=',') or (ch='.') then ch:=separator;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;

  result:=StrToFloat(s);
  if (_pos<=Length(_str)) and isDelimiter(_str[_pos]) then inc(_pos);
  //    inc(_pos);
end;

function TSimpleParser.getInt: Integer;
var ch: char;
    s: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'-') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;
  if s='' then result:=0 else result:=StrToInt(s);
  if (_pos<=Length(_str)) and isDelimiter(_str[_pos]) then inc(_pos);
//  inc(_pos);
end;

function TSimpleParser.getInt64: Int64;
var ch: char;
    s: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and not isDelimiter(_str[_pos]) do begin
    ch:=_str[_pos];
    if ((ch>'9') or (ch<'0')) and (ch<>'-') and (not isSpace(ch)) then break;
    if not isSpace(ch) then s:=s+ch;
    inc(_pos);
  end;
  if s='' then result:=0 else result:=StrToInt64(s);
end;

function TSimpleParser.getString: string;
begin
//  Result:=RightStr(_str,Length(_str)-_pos+1);
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and (not isDelimiter(_str[_pos])) do inc(_pos);
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
  inc(_pos);
end;

function TSimpleParser.getHex(digits: Integer=-1): Integer;
var x,Int0: Integer;
    ch: Integer;
begin
  int0:=Integer('0');
  x:=0;
  skip_spaces;
  fBackupPos:=_pos;
  while (_pos<=Length(_str)) and (_str[_pos]<='F') and (_str[_pos]>='0') and isIdentSymbol(_str[_pos]) and (digits<>0) do begin
    ch:=Integer(_str[_pos])-Int0;
    if ch>9 then ch:=ch-7;
    x:=(x shl 4) or ch;
    inc(_pos);
    dec(digits);
  end;
  Result:=x;
  if (_pos<=Length(_str)) and isDelimiter(_str[_pos]) then inc(_pos);
end;

function TSimpleParser.getBinary: Cardinal;
var Int0: Integer;
    ch: Cardinal;
    x: Cardinal;
begin
  int0:=Integer('0');
  x:=0;
  skip_spaces;
  fBackupPos:=_pos;
  while(_pos<=Length(_str)) and (_str[_pos]<='1') and (_str[_pos]>='0') do begin
    ch:=Integer(_str[_pos])-Int0;
    x:=(x shl 1) or ch;
    inc(_pos);
  end;
  Result:=x;
end;

function TSimpleParser.getIdent: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  if (_pos<=Length(_str)) and IsFirstIdentSymbol(_str[_pos]) then begin
    inc(_pos);
    while (_pos<=Length(_str)) and IsIdentSymbol(_str[_pos]) do inc(_pos);
  end;
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
end;

function TSimpleParser.getVarPathIdent: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  if (_pos<=Length(_str)) and IsFirstIdentSymbol(_str[_pos]) then begin
    inc(_pos);
    while (_pos<=Length(_str)) and (IsIdentSymbol(_str[_pos]) or (_str[_pos]='.')) do inc(_pos);
  end;
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
end;

function TSimpleParser.getPhysUnitIdent: string;
begin
  skip_spaces;
  fBackupPos:=_pos;
  if (_pos<=Length(_str)) and IsPhysIdentSymbol(_str[_pos]) then begin
    inc(_pos);
    while (_pos<=Length(_str)) and IsPhysIdentSymbol(_str[_pos]) do inc(_pos);
  end;
  Result:=MidStr(_str,fBackupPos,_pos-fBackupPos);
end;

end.
