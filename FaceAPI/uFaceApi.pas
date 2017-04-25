unit uFaceApi;

interface

uses
  System.Classes;

type
  TFaceApiServer = (fasGeneral);

  TFaceApiBase = class
  private
    FAccessKey: String;
  public
    procedure SetAccessKey(AValue: String);
    procedure SetAccessServer(AValue: TFaceApiServer);
  end;

  TFaceApi = class(TFaceApiBase)
  public
//    function Detect(AURL: String; AReturnFaceId, AReturnFaceLandmarks: Boolean): String;
    function Detect(AFileName: String; AReturnFaceId, AReturnFaceLandmarks: Boolean): String; overload;
    function Detect(AFile: TMemoryStream; AReturnFaceId, AReturnFaceLandmarks: Boolean): String; overload;

    constructor Create(AAccessKey: String);
  end;

implementation

{ TFaceApi }

function TFaceApi.Detect(AFileName: String; AReturnFaceId, AReturnFaceLandmarks: Boolean): String;
begin

end;

constructor TFaceApi.Create(AAccessKey: String);
begin
  inherited Create;

  FAccessKey := AAccessKey;
end;

function TFaceApi.Detect(AFile: TMemoryStream; AReturnFaceId, AReturnFaceLandmarks: Boolean): String;
begin

end;


{ TFaceApiBase }

procedure TFaceApiBase.SetAccessKey(AValue: String);
begin

end;

procedure TFaceApiBase.SetAccessServer(AValue: TFaceApiServer);
begin

end;

end.
