unit uFunctions.FaceApiHelper;

interface

uses
  { TFaceApiServer }
  uFaceApi.Servers.Types,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TBytesStream }
  System.Classes;

type
  FaceApiHelper = class
    class function CreateNewPerson(AAccessKey: String; AAccessServer: TFaceApiServer; AGroupID: String; APersonName: String; APersonUserData: String): String;

    class function DetectURL(AAccessKey: String; AAccessServer: TFaceApiServer; const AURL: String; const ADetectOptions: TDetectOptions): String;
    class function DetectFile(AAccessKey: String; AAccessServer: TFaceApiServer; const AFileName: String; const ADetectOptions: TDetectOptions): String;
    class function DetectStream(AAccessKey: String; AAccessServer: TFaceApiServer; AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;

    class function ListPersonGroups(AAccessKey: String; AAccessServer: TFaceApiServer; const AStart: String = ''; const ATop: Integer = 1000): String;
    class function ListPersonsInPersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;

    class function TrainPersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;
    class function GetPersonGroupTrainingStatus(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;

    class function CreatePersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String; const AGroupUserData: String): String;
    class function DeletePersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;
  end;

implementation

uses
  { IFaceApi }
  uIFaceApi,
  { TFaceApi }
  uFaceApi;

class function FaceApiHelper.CreateNewPerson(AAccessKey: String; AAccessServer: TFaceApiServer; AGroupID: String; APersonName: String; APersonUserData: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.CreatePerson(AGroupID, APersonName, APersonUserData);
end;

class function FaceApiHelper.CreatePersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID, AGroupUserData: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.CreatePersonGroup(AGroupID, AGroupUserData);
end;

class function FaceApiHelper.DeletePersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.DeletePersonGroup(AGroupID);
end;

class function FaceApiHelper.DetectFile(AAccessKey: String; AAccessServer: TFaceApiServer; const AFileName: String; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.DetectFile(AFileName, ADetectOptions);
end;

class function FaceApiHelper.DetectStream(AAccessKey: String; AAccessServer: TFaceApiServer; AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
  LRequestContent: TStringStream;
begin
  LRequestContent := nil;

  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.DetectStream(AStream, ADetectOptions);
end;

class function FaceApiHelper.DetectURL(AAccessKey: String; AAccessServer: TFaceApiServer; const AURL: String; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.DetectURL(AURL, ADetectOptions);
end;

class function FaceApiHelper.GetPersonGroupTrainingStatus(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.GetPersonGroupTrainingStatus(AGroupID);
end;

class function FaceApiHelper.ListPersonGroups(AAccessKey: String; AAccessServer: TFaceApiServer; const AStart: String = ''; const ATop: Integer = 1000): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.ListPersonGroups(AStart, ATop);
end;

class function FaceApiHelper.ListPersonsInPersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.ListPersonsInPersonGroup(AGroupID);
end;

class function FaceApiHelper.TrainPersonGroup(AAccessKey: String; AAccessServer: TFaceApiServer; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccessKey, AAccessServer);

  Result := LIFaceApi.TrainPersonGroup(AGroupID);
end;

end.
