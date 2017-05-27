/// <summary>
///   Contain Main class with FaceAPI async functions for end-user
/// </summary>
unit uFunctions.FaceApiAsyncHelper;

interface

uses
  { TTask.Create; TProc }
  System.SysUtils,
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TBytesStream }
  System.Classes,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  /// <summary>
  ///   Type of callback procedure with result for async functions
  /// </summary>
  TFaceApiAsyncCallback = procedure(const AResult: String) of object;

  /// <summary>
  ///   Main class with FaceAPI async functions for end-user
  /// </summary>
  FaceApiAsyncHelper = class
  private
    class procedure RunAsync(AProc: TProc);

    class procedure CallCallback(ACallbackMethod: TFaceApiAsyncCallback = nil;
      const AResult: String = '');
  public
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectURL">interface DetectURL</see>
    /// </summary>
    class function DetectURL(AAccess: TAccessServer; const AURL: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectFile">interface DetectFile</see>
    /// </summary>
    class function DetectFile(AAccess: TAccessServer; const AFileName: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectStream">interface DetectStream</see>
    /// </summary>
    class function DetectStream(AAccess: TAccessServer; AStream: TBytesStream;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    class function ListPersonGroups(AAccess: TAccessServer;
      ACallbackMethod: TFaceApiAsyncCallback = nil;
      const AStart: String = ''; const ATop: Integer = 1000): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    class function ListPersonsInPersonGroup(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.AddPersonFaceURL">interface AddPersonFaceURL</see>
    /// </summary>
    class function AddPersonFaceURL(AAccess: TAccessServer; const AGroupID, APersonID,
      AURL,  ATargetFace: String; const AUserData: String = '';
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.CreatePerson">interface CreatePerson</see>
    /// </summary>
    class function CreatePerson(AAccess: TAccessServer; AGroupID: String;
      APersonName: String; APersonUserData: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.GetPersonGroupTrainingStatus">interface GetPersonGroupTrainingStatus</see>
    /// </summary>
    class function GetPersonGroupTrainingStatus(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.TrainPersonGroup">interface TrainPersonGroup</see>
    /// </summary>
    class function TrainPersonGroup(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.CreatePersonGroup">interface CreatePersonGroup</see>
    /// </summary>
    class function CreatePersonGroup(AAccess: TAccessServer;
      const AGroupID, AGroupName, AGroupUserData: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.DeletePersonGroup">interface DeletePersonGroup</see>
    /// </summary>
    class function DeletePersonGroup(AAccess: TAccessServer;
      const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Verify">interface Verify (overload)</see>
    /// </summary>
    class function Verify(AAccess: TAccessServer;
      const AFaceTempID1, AFaceTempID2: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String; overload;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Verify">interface Verify (overload)</see>
    /// </summary>
    class function Verify(AAccess: TAccessServer;
      const AFaceTempID, APersonID, AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String; overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Identify">interface Identify</see>
    /// </summary>
    class function Identify(AAccess: TAccessServer; AFaceIDS: TStringList;
      const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer = 1;
      const AConfidenceThreshold: Double = 0.5;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.UpdatePersonGroup">interface UpdatePersonGroup</see>
    /// </summary>
    class function UpdatePersonGroup(AAccess: TAccessServer;
      const AGroupID: String; const AGroupName: String;
      const AGroupUserData: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.GetPersonGroup">interface GetPersonGroup</see>
    /// </summary>
    class function GetPersonGroup(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar (overload)</see>
    /// </summary>
    class function FindSimilar(AAccess: TAccessServer; const AFaceID: String;
      const AListID: String; const AMaxNumOfCandidatesReturned: Integer = 20;
      AFindMode: String = 'matchPerson';
      ACallbackMethod: TFaceApiAsyncCallback = nil): String; overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar (overload)</see>
    /// </summary>
    class function FindSimilar(AAccess: TAccessServer; const AFaceID: String;
      AFaceIDS: TStringList; const AMaxNumOfCandidatesReturned: Integer = 20;
      AFindMode: String = 'matchPerson';
      ACallbackMethod: TFaceApiAsyncCallback = nil): String; overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Group">interface Group</see>
    /// </summary>
    class function Group(AAccess: TAccessServer; AFaceIDS: TStringList;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;
  end;

implementation

uses
  { IFaceApiCore }
  uIFaceApi.Core,
  { TFaceApiCore }
  uFaceApi.Core,
  { IFaceApiFace }
  uIFaceApi.Face,
  { ITask }
  System.Threading,
  { FaceApiHelper }
  uFunctions.FaceApiHelper;

class procedure FaceApiAsyncHelper.RunAsync(AProc: TProc);
var
  LTask: ITask;
begin
  LTask := TTask.Create(
    procedure
    begin
      AProc;
    end
  );
  LTask.Start;
end;

class procedure FaceApiAsyncHelper.CallCallback(ACallbackMethod: TFaceApiAsyncCallback = nil;
  const AResult: String = '');
begin
  if Assigned(ACallbackMethod) then
    ACallbackMethod(AResult);
end;

class function FaceApiAsyncHelper.AddPersonFaceURL(AAccess: TAccessServer;
  const AGroupID, APersonID, AURL, ATargetFace, AUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin

end;

class function FaceApiAsyncHelper.CreatePerson(AAccess: TAccessServer; AGroupID,
  APersonName, APersonUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.CreatePerson(AAccess, AGroupID, APersonName, APersonUserData);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.CreatePersonGroup(AAccess: TAccessServer;
  const AGroupID, AGroupName, AGroupUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback = nil): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.CreatePerson(AAccess, AGroupID, AGroupName, AGroupUserData);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.DeletePersonGroup(AAccess: TAccessServer;
  const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback = nil): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.DeletePersonGroup(AAccess, AGroupID);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.DetectFile(AAccess: TAccessServer;
  const AFileName: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback = nil): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.DetectFile(AAccess, AFileName, ADetectOptions);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.DetectStream(AAccess: TAccessServer;
  AStream: TBytesStream; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback = nil): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.DetectStream(AAccess, AStream, ADetectOptions);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.DetectURL(AAccess: TAccessServer;
  const AURL: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback = nil): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.DetectURL(AAccess, AURL, ADetectOptions);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.FindSimilar(AAccess: TAccessServer;
  const AFaceID, AListID: String; const AMaxNumOfCandidatesReturned: Integer;
  AFindMode: String; ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.FindSimilar(AAccess, AFaceID, AListID,
        AMaxNumOfCandidatesReturned, AFindMode);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.FindSimilar(AAccess: TAccessServer;
  const AFaceID: String; AFaceIDS: TStringList;
  const AMaxNumOfCandidatesReturned: Integer; AFindMode: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.FindSimilar(AAccess, AFaceID, AFaceIDS,
        AMaxNumOfCandidatesReturned, AFindMode);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.GetPersonGroup(AAccess: TAccessServer;
  const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.GetPersonGroup(AAccess, AGroupID);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.GetPersonGroupTrainingStatus(
  AAccess: TAccessServer; const AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.GetPersonGroupTrainingStatus(AAccess, AGroupID);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.Group(AAccess: TAccessServer;
  AFaceIDS: TStringList; ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.Group(AAccess, AFaceIDS);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.Identify(AAccess: TAccessServer;
  AFaceIDS: TStringList; const AGroupID: String;
  const AMaxNumOfCandidatesReturned: Integer;
  const AConfidenceThreshold: Double;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.Identify(AAccess, AFaceIDS, AGroupID,
        AMaxNumOfCandidatesReturned, AConfidenceThreshold);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.ListPersonGroups(AAccess: TAccessServer;
  ACallbackMethod: TFaceApiAsyncCallback = nil;
  const AStart: String = ''; const ATop: Integer = 1000): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.ListPersonGroups(AAccess, AStart, ATop);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.ListPersonsInPersonGroup(
  AAccess: TAccessServer; const AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback = nil): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.ListPersonsInPersonGroup(AAccess, AGroupID);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.TrainPersonGroup(AAccess: TAccessServer;
  const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.TrainPersonGroup(AAccess, AGroupID);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.UpdatePersonGroup(AAccess: TAccessServer;
  const AGroupID, AGroupName, AGroupUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.UpdatePersonGroup(AAccess, AGroupID, AGroupName,
        AGroupUserData);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.Verify(AAccess: TAccessServer;
  const AFaceTempID1, AFaceTempID2: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.Verify(AAccess, AFaceTempID1, AFaceTempID2);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class function FaceApiAsyncHelper.Verify(AAccess: TAccessServer;
  const AFaceTempID, APersonID, AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.Verify(AAccess, AFaceTempID, APersonID, AGroupID);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

end.
