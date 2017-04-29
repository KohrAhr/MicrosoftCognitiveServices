unit uFaceApi.FaceDetectOptions;

interface

uses
  { TFaceAttributes }
  uFaceApi.FaceAttributes;

type
  TDetectOptions = record
    FaceId: Boolean;
    FaceLandmarks: Boolean;
    FaceAttributes: TFaceAttributes;

    function FaceAttributesToString: String;
    constructor Create(AFaceId: Boolean; AFaceLandmarks: Boolean = False; AFaceAttributes: TFaceAttributes = []);
  end;

  function Detect(AFaceId: Boolean; AFaceLandmarks: Boolean = False; AFaceAttributes: TFaceAttributes = []): TDetectOptions;

implementation

function Detect(AFaceId: Boolean; AFaceLandmarks: Boolean = False; AFaceAttributes: TFaceAttributes = []): TDetectOptions;
begin
  Result := TDetectOptions.Create(AFaceId, AFaceLandmarks, AFaceAttributes);
end;

constructor TDetectOptions.Create(AFaceId: Boolean; AFaceLandmarks: Boolean = False; AFaceAttributes: TFaceAttributes = []);
begin
  FaceId := AFaceId;
  FaceLandmarks := AFaceLandmarks;
  FaceAttributes := AFaceAttributes;
end;

function TDetectOptions.FaceAttributesToString: String;
var
  LFaceAttribute: TFaceAttribute;
begin
  Result := '';

  for LFaceAttribute := Low(CONST_FACE_ATTRIBUTES) to High(CONST_FACE_ATTRIBUTES) do
    if LFaceAttribute in FaceAttributes then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + CONST_FACE_ATTRIBUTES[LFaceAttribute];
      end;
end;

end.
