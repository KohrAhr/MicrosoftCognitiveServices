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
    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.DetectURL">interface DetectURL</see>
    /// </summary>
    class function DetectURL(AAccess: TAccess; const AURL: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.DetectFile">interface DetectFile</see>
    /// </summary>
    class function DetectFile(AAccess: TAccess; const AFileName: String; const ADetectOptions: TDetectOptions): String;
    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.DetectStream">interface DetectStream</see>
    /// </summary>
    class function DetectStream(AAccess: TAccess; AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;

    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    class function ListPersonGroups(AAccess: TAccess; const AStart: String = ''; const ATop: Integer = 1000): String;

    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    class function ListPersonsInPersonGroup(AAccess: TAccess; const AGroupID: String): String;

    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.CreatePerson">interface CreatePerson</see>
    /// </summary>
    class function CreateNewPerson(AAccess: TAccess; AGroupID: String; APersonName: String; APersonUserData: String): String;

    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.GetPersonGroupTrainingStatus">interface GetPersonGroupTrainingStatus</see>
    /// </summary>
    class function GetPersonGroupTrainingStatus(AAccess: TAccess; const AGroupID: String): String;
    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.TrainPersonGroup">interface TrainPersonGroup</see>
    /// </summary>
    class function TrainPersonGroup(AAccess: TAccess; const AGroupID: String): String;

    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.CreatePersonGroup">interface CreatePersonGroup</see>
    /// </summary>
    class function CreatePersonGroup(AAccess: TAccess; const AGroupID: String; const AGroupUserData: String): String;

    /// <summary>
    ///   Implements top layout for <see cref="uIFaceApi|IFaceApi.DeletePersonGroup">interface DeletePersonGroup</see>
    /// </summary>
    class function DeletePersonGroup(AAccess: TAccess; const AGroupID: String): String;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Verify">interface Verify (overload)</see>
    /// </summary>
    class function Verify(AAccess: TAccess; const AFaceTempID1, AFaceTempID2: String): String; overload;
    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Verify">interface Verify (overload)</see>
    /// </summary>
    class function Verify(AAccess: TAccess; const AFaceTempID, APersonID, AGroupID: String): String; overload;

    /// <summary>
    ///   Implements <see cref="uIFaceApi|IFaceApi.Identify">interface Identify</see>
    /// </summary>
    class function Identify(AAccess: TAccess; AFaceIDS: TStringList; const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer = 1; const AConfidenceThreshold: Double = 0.5): String;

  end;

implementation

uses
  { IFaceApi }
  uIFaceApi,
  { TFaceApiCore }
  uFaceApi.Core;

class function FaceApiHelper.CreateNewPerson(AAccess: TAccess; AGroupID: String; APersonName: String; APersonUserData: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.CreatePerson(AGroupID, APersonName, APersonUserData);
end;

class function FaceApiHelper.CreatePersonGroup(AAccess: TAccess; const AGroupID, AGroupUserData: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.CreatePersonGroup(AGroupID, AGroupUserData);
end;

class function FaceApiHelper.DeletePersonGroup(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DeletePersonGroup(AGroupID);
end;

class function FaceApiHelper.DetectFile(AAccess: TAccess; const AFileName: String; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DetectFile(AFileName, ADetectOptions);
end;

class function FaceApiHelper.DetectStream(AAccess: TAccess; AStream: TBytesStream; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
  LRequestContent: TStringStream;
begin
  LRequestContent := nil;

  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DetectStream(AStream, ADetectOptions);
end;

class function FaceApiHelper.DetectURL(AAccess: TAccess; const AURL: String; const ADetectOptions: TDetectOptions): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.DetectURL(AURL, ADetectOptions);
end;

class function FaceApiHelper.GetPersonGroupTrainingStatus(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.GetPersonGroupTrainingStatus(AGroupID);
end;

class function FaceApiHelper.ListPersonGroups(AAccess: TAccess; const AStart: String = ''; const ATop: Integer = 1000): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.ListPersonGroups(AStart, ATop);
end;

class function FaceApiHelper.ListPersonsInPersonGroup(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.ListPersonsInPersonGroup(AGroupID);
end;

class function FaceApiHelper.TrainPersonGroup(AAccess: TAccess; const AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.TrainPersonGroup(AGroupID);
end;

class function FaceApiHelper.Verify(AAccess: TAccess; const AFaceTempID1, AFaceTempID2: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.Verify(AFaceTempID1, AFaceTempID2);
end;

class function FaceApiHelper.Verify(AAccess: TAccess; const AFaceTempID, APersonID, AGroupID: String): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.Verify(AFaceTempID, APersonID, AGroupID);
end;

class function FaceApiHelper.Identify(AAccess: TAccess; AFaceIDS: TStringList; const AGroupID: String;
  const AMaxNumOfCandidatesReturned: Integer; const AConfidenceThreshold: Double): String;
var
  LIFaceApi: IFaceApi;
begin
  LIFaceApi := TFaceApiCore.Create;

  LIFaceApi.SetAccessKey(AAccess);

  Result := LIFaceApi.Identify(AFaceIDS, AGroupID, AMaxNumOfCandidatesReturned, AConfidenceThreshold);
end;


end.
