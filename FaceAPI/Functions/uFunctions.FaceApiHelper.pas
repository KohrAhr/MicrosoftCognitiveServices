unit uFunctions.FaceApiHelper;

interface

uses
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TBytesStream }
  System.Classes,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  FaceApiHelper = class
    class function CreateNewPerson(AAccess: TAccess; AGroupID: String; APersonName: String; APersonUserData: String): String;

    class function DetectURL(AAccess: TAccess; const AURL: String; const ADetectOptions: TDetectOptions): String;
    class function DetectFile(AAccess: TAccess; const AFileName: String; const ADetectOptions: TDetectOptions): String;
    class function DetectStream(AAccess: TAccess; AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;

    class function ListPersonGroups(AAccess: TAccess; const AStart: String = ''; const ATop: Integer = 1000): String;
    class function ListPersonsInPersonGroup(AAccess: TAccess; const AGroupID: String): String;

    class function TrainPersonGroup(AAccess: TAccess; const AGroupID: String): String;
    class function GetPersonGroupTrainingStatus(AAccess: TAccess; const AGroupID: String): String;

    class function CreatePersonGroup(AAccess: TAccess; const AGroupID: String; const AGroupUserData: String): String;
    class function DeletePersonGroup(AAccess: TAccess; const AGroupID: String): String;
  end;

implementation

uses
  { IFaceApi }
  uIFaceApi,
  { TFaceApi }
  uFaceApi;

class function FaceApiHelper.CreateNewPerson(AAccess: TAccess; AGroupID: String; APersonName: String; APersonUserData: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.CreatePerson(AGroupID, APersonName, APersonUserData);
end;

class function FaceApiHelper.CreatePersonGroup(AAccess: TAccess; const AGroupID, AGroupUserData: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.CreatePersonGroup(AGroupID, AGroupUserData);
end;

class function FaceApiHelper.DeletePersonGroup(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DeletePersonGroup(AGroupID);
end;

class function FaceApiHelper.DetectFile(AAccess: TAccess; const AFileName: String; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DetectFile(AFileName, ADetectOptions);
end;

class function FaceApiHelper.DetectStream(AAccess: TAccess; AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
  LRequestContent: TStringStream;
begin
  LRequestContent := nil;

  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DetectStream(AStream, ADetectOptions);
end;

class function FaceApiHelper.DetectURL(AAccess: TAccess; const AURL: String; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DetectURL(AURL, ADetectOptions);
end;

class function FaceApiHelper.GetPersonGroupTrainingStatus(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.GetPersonGroupTrainingStatus(AGroupID);
end;

class function FaceApiHelper.ListPersonGroups(AAccess: TAccess; const AStart: String = ''; const ATop: Integer = 1000): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.ListPersonGroups(AStart, ATop);
end;

class function FaceApiHelper.ListPersonsInPersonGroup(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.ListPersonsInPersonGroup(AGroupID);
end;

class function FaceApiHelper.TrainPersonGroup(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApi.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.TrainPersonGroup(AGroupID);
end;

end.
