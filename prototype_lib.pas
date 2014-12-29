unit prototype_lib;

interface

uses classes,streaming_class_lib, contnrs;

type

TPrototypedObject=class(TStreamingClass)
  private
    fPrototype: TPrototypedObject;
    function GetPrototypeName: string;
    procedure SetPrototypeName(value: string);
//    procedure SetPrototype(value: TPrototypedObject);
  protected
    function HasPrototype: Boolean;
    function IsPrototype: boolean;
  published
    property Prototype: string read GetPrototypeName write SetPrototypeName stored HasPrototype;
end;

TPrototypeClass=class of TPrototypedObject;

procedure LoadPrototypes(dir,FileMask: string);

var
  PrototypeList: TObjectList;


implementation

uses SysUtils;

function TPrototypedObject.GetPrototypeName: string;
begin
  Result:=fPrototype.Name;
end;

procedure TPrototypedObject.SetPrototypeName(value: string);
var i: Integer;
    buName: string;
begin
  for i:=0 to PrototypeList.Count-1 do
    if (PrototypeList[i] is ClassType) and ((PrototypeList[i] as TPrototypedObject).name=value) then begin
      fPrototype:=TPrototypedObject(PrototypeList[i]);
      buName:=Name;
      Assign(fPrototype);
      Name:=buName;
      Exit;
    end;
  fPrototype:=nil;
  Raise Exception.CreateFmt('SetPrototypeName: couldn''t find prototype %s',[value]);
end;

function TPrototypedObject.IsPrototype: boolean;
begin
  Result:=(fPrototype=nil);
end;

function TPrototypedObject.HasPrototype: Boolean;
begin
  Result:=(fPrototype<>nil);
end;

(*
    General
                *)
procedure LoadPrototypes(dir,FileMask: string);
var sr: TSearchRec;
    fn: string;
    p: TPrototypedObject;
    budir: string;
begin
  budir:=GetCurrentDir;
  SetCurrentDir(dir);
  fn:=FileMask;
  if FindFirst(fn,faAnyFile,sr)=0 then repeat
    try
      p:=TPrototypedObject.LoadComponentFromFile(sr.Name) as TPrototypedObject;
      PrototypeList.Add(p);
    finally

    end;
  until FindNext(sr)<>0;
  FindClose(sr);
  SetCurrentDir(budir);
end;


initialization
  PrototypeList:=TObjectList.Create;

finalization
  PrototypeList.Free;

end.
