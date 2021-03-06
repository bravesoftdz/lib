unit BEncode;

interface

uses
  Classes, Contnrs, SysUtils;

type
  TBEncodedFormat = (befEmpty, befString, befInteger, befList, befDictionary);
  TBEncodedData = class(TObject)
  public
    Header: string;
    Data: TObject; // actually TBEncoded
    destructor Destroy; override;
  published
    constructor Create(aData: TObject);
  end;
  TBEncodedDataList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TBEncodedData;
    procedure SetItems(Index: Integer; AClass: TBEncodedData);
  public
    function FindElement(Header: string): TObject;
    function Add(AClass: TBEncodedData): Integer;
    function Extract(Item: TBEncodedData): TBEncodedData;
    function Remove(AClass: TBEncodedData): Integer;
    function IndexOf(AClass: TBEncodedData): Integer;
    function First: TBEncodedData;
    function Last: TBEncodedData;
    procedure Insert(Index: Integer; AClass: TBEncodedData);
    property Items[Index: Integer]: TBEncodedData read GetItems write SetItems;
      default;
  end;
  TBEncoded = class(TObject)
  private
    FFormat: TBEncodedFormat;
    procedure SetFormat(Format: TBEncodedFormat);
  public
    StringData: string;
    IntegerData: int64;
    ListData: TBEncodedDataList;
    property Format: TBEncodedFormat read FFormat write SetFormat;
    class procedure Encode(Encoded: TObject; var Output: string);
    destructor Destroy; override;
  published
    constructor Create(Stream: TStream);
  end;

implementation

(*
    TBEncodedData
                        *)

destructor TBEncodedData.Destroy;
begin
  Data.Free;
  inherited Destroy;
end;

constructor TBEncodedData.Create(aData: TObject);
begin
  inherited Create;

  Data := aData;
end;

(*
    TBEncoded
                  *)
constructor TBEncoded.Create(Stream: TStream);

  function GetString(Buffer: string): string;
  var
    X: char;
  begin
    // loop until we come across it
    repeat
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('');
      if not ((X in ['0'..'9']) or (x = ':')) then
        raise Exception.Create('');
      if X = ':' then
      begin
        if Buffer = '' then
          raise Exception.Create('');
        if Length(Buffer) > 6 then
          raise Exception.Create('');
        SetLength(Result, StrToInt(Buffer));
        if Stream.Read(Result[1], Length(Result)) <> Length(Result) then
          raise Exception.Create('');
        Break;
      end
      else
        Buffer := Buffer + X;
    until False;
  end;

var
  X: char;
  Buffer: string;
  Data: TBEncodedData;
  Encoded: TBEncoded;
begin
  inherited Create;

  // get first character to determine the format of the proceeding data
  if Stream.Read(X, 1) <> 1 then
    raise Exception.Create('Unexpected end of file');

  // is it an integer?
  if X = 'i' then
  begin
    // yes it is, let's read until we come across e
    Buffer := '';
    repeat
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('unexpected end of file');
      if not ((X in ['0'..'9']) or (X = 'e')) then
        raise Exception.CreateFMT('0..9 or e expected, %c found',[X]);
      if X = 'e' then
      begin
        if Buffer = '' then
          raise Exception.Create('empty integer')
        else
        begin
          Format := befInteger;
          IntegerData := StrToInt64(Buffer);
          Break;
        end;
      end
      else
        Buffer := Buffer + X;
    until False;
  end
  // is it a list?
  else if X = 'l' then
  begin
    // its a list
    Format := befList;

    // loop until we come across e
    repeat
      // have a peek around and see if theres an e
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('unexpected end of file');
      // is it an e?
      if X = 'e' then
        Break;
      // otherwise move the cursor back
      Stream.Seek(-1, soFromCurrent);
      // create the element
      Encoded := TBEncoded.Create(Stream);
      // add it to the list
      ListData.Add(TBEncodedData.Create(Encoded));
    until False;
  end
  // is it a dictionary?
  else if X = 'd' then
  begin
    // its a dictionary :>
    Format := befDictionary;

    // loop until we come across e
    repeat
      // have a peek around and see if theres an e
      if Stream.Read(X, 1) <> 1 then
        raise Exception.Create('');
      // is it an e?
      if X = 'e' then
        Break;
      // if it isnt an e it has to be numerical!
      if not (X in ['0'..'9']) then
        raise Exception.Create('');
      // now read the string data
      Buffer := GetString(string(X));
      // create the element
      Encoded := TBEncoded.Create(Stream);
      // create the data element
      Data := TBEncodedData.Create(Encoded);
      Data.Header := Buffer;
      // add it to the list
      ListData.Add(Data);
    until False;
  end
  // is it a string?
  else if X in ['0'..'9'] then
  begin
    StringData := GetString(string(X));
    Format := befString;
  end
  else
    raise Exception.Create('');
end;


destructor TBEncoded.Destroy;
begin
  ListData.Free;
  inherited Destroy;
end;


class procedure TBEncoded.Encode(Encoded: TObject; var Output: string);
var
  i: integer;
begin
  with Encoded as TBEncoded do
  begin
    // what type of member is it?
    case Format of
      befString: Output := Output + IntToStr(Length(StringData)) + ':' +
        StringData;
      befInteger: Output := Output + 'i' + IntToStr(IntegerData) + 'e';
      befList:
      begin
        Output := Output + 'l';
        for i := 0 to ListData.Count - 1 do
          Encode(TBEncoded(ListData[i].Data), Output);
        Output := Output + 'e';
      end;
      befDictionary:
      begin
        Output := Output + 'd';
        for i := 0 to ListData.Count - 1 do
        begin
          Output := Output + IntToStr(Length(ListData[i].Header)) + ':' +
            ListData[i].Header;
          Encode(TBEncoded(ListData[i].Data), Output);
        end;
        Output := Output + 'e';
      end;
    end;
  end;
end;

procedure TBEncoded.SetFormat(Format: TBEncodedFormat);
begin
  if Format in [befList, befDictionary] then
    ListData := TBEncodedDataList.Create;
  FFormat := Format;
end;

(*
    TBEncodedDataList
                          *)

function TBEncodedDataList.FindElement(Header: string): TObject;
var
  i: integer;
begin
  Header := LowerCase(Header);
  for i := 0 to Count - 1 do
    if LowerCase(Items[i].Header) = Header then
    begin
      Result := Items[i].Data;
      Exit;
    end;

  Result := nil;
end;

function TBEncodedDataList.Add(AClass: TBEncodedData): Integer;
begin
  Result := inherited Add(AClass);
end;

function TBEncodedDataList.Extract(Item: TBEncodedData): TBEncodedData;
begin
  Result := TBEncodedData(inherited Extract(Item));
end;

function TBEncodedDataList.First: TBEncodedData;
begin
  Result := TBEncodedData(inherited First);
end;

function TBEncodedDataList.GetItems(Index: Integer): TBEncodedData;
begin
  Result := TBEncodedData(inherited Items[Index]);
end;

function TBEncodedDataList.IndexOf(AClass: TBEncodedData): Integer;
begin
  Result := inherited IndexOf(AClass);
end;

procedure TBEncodedDataList.Insert(Index: Integer; AClass: TBEncodedData);
begin
  inherited Insert(Index, AClass);
end;

function TBEncodedDataList.Last: TBEncodedData;
begin
  Result := TBEncodedData(inherited First);
end;

function TBEncodedDataList.Remove(AClass: TBEncodedData): Integer;
begin
  Result := inherited Remove(AClass);
end;

procedure TBEncodedDataList.SetItems(Index: Integer; AClass: TBEncodedData);
begin
  inherited Items[Index] := AClass;
end;

end.
