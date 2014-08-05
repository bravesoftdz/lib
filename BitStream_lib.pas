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
  end;

TWriteBitStream=class(TBitStream)
  public
    destructor Destroy; override;
    procedure WriteBits(source: Integer; count: Integer);
  end;

TReadBitStream=class(TBitStream)
  public
    function ReadBits(out dest: Integer; count: Integer): Integer;
  end;


implementation

constructor TBitStream.Create(stream: TStream);
begin
  inherited Create;
  fStream:=stream;
end;

procedure TWriteBitStream.WriteBits(source: Integer; count: Integer);
var mask: Integer;
begin
  if count<1 then Exit;
//���������� �� 0 �� 32 ��� � ���. �����
  mask:=(1 shl count)-1;
  source:=source and mask;
  if fcapacity+count<32 then begin
    fBitBuffer:=(fBitBuffer shl count) or source;
    inc(fCapacity,count);
  end
  else begin
    fBitBuffer:=(fBitBuffer shl (32-fcapacity)) or (source shr (count+fcapacity-32));
    //������ fBitBuffer �����!
    fStream.Write(fBitBuffer,SizeOf(fBitBuffer));
    mask:=mask shr (32-fcapacity);
    fBitBuffer:=source and mask;
    fCapacity:=fcapacity+count-32;
  end;
end;

destructor TWriteBitStream.Destroy;
begin
  //����� �������� ��������� �����
  fBitBuffer:=fBitBuffer shl (32-fcapacity);
  fStream.Write(fBitBuffer,SizeOf(fBitBuffer));
  inherited Destroy;
end;


function TReadBitStream.ReadBits(out dest: Integer; count: Integer): Integer;
var mask: Integer;
    item: Integer;
    bucount: Integer;
begin
  dest:=0;
  Result:=0;
  bucount:=count;
  if fCapacity<count then begin
    mask:=(1 shl fCapacity)-1;  //�� ������� ����, ������� � �������
    dest:=fBitBuffer and (mask shl (32-fCapacity));
    //������ ����� arithmetic shift right (sar)
    //dest:=dest sar (32-count)
//    dest:=dest shr (32-count);
    dest:=dest div (1 shl (32-count));
    Result:=fCapacity;
    count:=count-fCapacity;
    fCapacity:=fstream.Read(fBitBuffer,SizeOf(fBitBuffer))*8;
    //���� fCapacity<count, ��� �������� ���� ���������
    if fCapacity<count then count:=fCapacity;
  end;
  mask:=(1 shl count)-1;
  item:=fBitBuffer and (mask shl (32-count));
  //������ ��-�������� ����� ���������� arithmetic shift right
  //dest:=dest sar (32-count);
//  dest:=dest shr (32-count);
  if count=bucount then
    item:=item div (1 shl (32-count))
  else
    item:=item shr (32-count);
  dest:=dest or item;
  fBitBuffer:=fBitBuffer shl count;
  dec(fCapacity,count);
  Result:=Result+Count;
end;



end.
