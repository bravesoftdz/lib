unit PNGImageList;

interface

uses classes,controls;

type

TPngImageList=class(TImageList)
  protected
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  end;

procedure Register;

implementation

uses PNGImage,graphics;

procedure Register;
begin
  RegisterComponents('CautiousEdit',[TPngImageList]);
end;


procedure TPngImageList.ReadData(Stream: TStream);
var i: Integer;
    img: TPngObject;
    btmp: TBitmap;
begin
//  inherited WriteData(Stream);
  while Stream.Position<Stream.Size do begin
  img:=TPngObject.Create;
  btmp:=TBitmap.Create;

    img.LoadFromStream(Stream);

    img.AssignTo(btmp);
    Add(btmp,nil);
      btmp.Free;
  img.Free;

  end;
end;

procedure TPngImageList.WriteData(Stream: TStream);
var i: Integer;
    img: TPngObject;
    btmp: TBitmap;
begin
//  inherited WriteData(Stream);
  img:=TPngObject.Create;
  img.CompressionLevel:=9;
  img.Filters:=[pfNone, pfSub, pfUp, pfAverage, pfPaeth];
  btmp:=TBitmap.Create;
  for i:=0 to Count-1 do begin
    btmp.Width:=0;
    btmp.Height:=0;
    GetBitmap(i,btmp);
    img.Assign(btmp);
    img.SaveToStream(Stream);
  end;
  btmp.Free;
  img.Free;
end;

initialization
  RegisterClass(TPngImageList);

end.
