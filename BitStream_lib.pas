unit BitStream_lib;

interface

uses classes;

type

TBitStream=class(TStream)
  private
    fStream: TStream;
    fBitBuffer: Integer;
    fcapacity: Integer;
  public
    constructor Create(stream: Tstream);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

TWriteBitStream=class(TBitStream)
  public
    destructor Destroy; override;
    procedure WriteBits(source: Integer; count: Integer);
//    procedure WriteBitsUnsigned(source: Cardinal; count: Integer);
  end;

TReadBitStream=class(TBitStream)
  public
    function ReadBitsSigned(out dest: Integer; count: Integer): Integer;
    function ReadBitsUnsigned(out dest: Cardinal; count: Integer): Integer;
  end;


implementation
(*
      TBitStream
                        *)
constructor TBitStream.Create(stream: TStream);
begin
  inherited Create;
  fStream:=stream;
end;

function TBitStream.Read(var Buffer; count: Integer): Integer;
begin
  Result:=fstream.Read(Buffer, count);
end;

function TBitStream.Write(const Buffer; count: Integer): Integer;
begin
  Result:=fstream.Write(Buffer,count);
end;

(*
    TWriteBitStream
                        *)
procedure TWriteBitStream.WriteBits(source: Integer; count: Integer);
var mask: Integer;
begin
  if count<1 then Exit;
//записывает от 0 до 32 бит в вых. поток
  mask:=(1 shl count)-1;
  source:=source and mask;
  if fcapacity+count<32 then begin
    fBitBuffer:=(fBitBuffer shl count) or source;
    inc(fCapacity,count);
  end
  else begin
    fBitBuffer:=(fBitBuffer shl (32-fcapacity)) or (source shr (count+fcapacity-32));
    //теперь fBitBuffer полон!
    fStream.Write(fBitBuffer,SizeOf(fBitBuffer));
    mask:=mask shr (32-fcapacity);
    fBitBuffer:=source and mask;
    fCapacity:=fcapacity+count-32;
  end;
end;

destructor TWriteBitStream.Destroy;
begin
  //нужно дописать последние байты
  fBitBuffer:=fBitBuffer shl (32-fcapacity);
  fStream.Write(fBitBuffer,SizeOf(fBitBuffer));
  inherited Destroy;
end;

(*
      TReadBitStream
                          *)
function TReadBitStream.ReadBitsSigned(out dest: Integer; count: Integer): Integer;
var mask: Integer;
    item: Integer;
    bucount: Integer;
begin
  dest:=0;
  Result:=0;
  bucount:=count;
  if fCapacity<count then begin
    mask:=(1 shl fCapacity)-1;  //уж сколько есть, столько и возьмем
    dest:=fBitBuffer and (mask shl (32-fCapacity));
    //сейчас нужен arithmetic shift right (sar)
    //dest:=dest sar (32-count)
//    dest:=dest shr (32-count);
    dest:=dest div (1 shl (32-count));
    if count=1 then dest:=-dest;
    Result:=fCapacity;
    count:=count-fCapacity;
    fCapacity:=fstream.Read(fBitBuffer,SizeOf(fBitBuffer))*8;
    //если fCapacity<count, нам остается лишь смириться
    if fCapacity<count then count:=fCapacity;
  end;
  mask:=(1 shl count)-1;
  item:=fBitBuffer and (mask shl (32-count));
  //сейчас по-хорошему нужно провернуть arithmetic shift right
  //dest:=dest sar (32-count);
//  dest:=dest shr (32-count);
  if count=bucount then begin
    item:=item div (1 shl (32-count));
    if count=1 then item:=-item;
  end
  else
    item:=item shr (32-count);
  dest:=dest or item;
  fBitBuffer:=fBitBuffer shl count;
  dec(fCapacity,count);
  Result:=Result+Count;
end;

function TReadBitStream.ReadBitsUnsigned(out dest: Cardinal; count: Integer): Integer;
var mask: Integer;
    item: Cardinal;
begin
  dest:=0;
  Result:=0;
  if fCapacity<count then begin
    mask:=(1 shl fCapacity)-1;  //уж сколько есть, столько и возьмем
    dest:=fBitBuffer and (mask shl (32-fCapacity));
    //сейчас нужен arithmetic shift right (sar)
    //dest:=dest sar (32-count)
//    dest:=dest shr (32-count);
    dest:=dest shr (1 shl (32-count));
    Result:=fCapacity;
    count:=count-fCapacity;
    fCapacity:=fstream.Read(fBitBuffer,SizeOf(fBitBuffer))*8;
    //если fCapacity<count, нам остается лишь смириться
    if fCapacity<count then count:=fCapacity;
  end;
  mask:=(1 shl count)-1;
  item:=fBitBuffer and (mask shl (32-count));
  //сейчас по-хорошему нужно провернуть arithmetic shift right
  //dest:=dest sar (32-count);
//  dest:=dest shr (32-count);
  item:=item shr (32-count);

  dest:=dest or item;
  fBitBuffer:=fBitBuffer shl count;
  dec(fCapacity,count);
  Result:=Result+Count;
end;

end.
