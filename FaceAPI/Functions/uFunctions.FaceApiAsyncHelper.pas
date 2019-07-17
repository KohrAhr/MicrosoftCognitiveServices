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
	uFaceApi.ServersAccess.Types,
	uFaceApi.Consts;

type
  /// <summary>
  ///   Type of callback procedure with result for async functions
  /// </summary>
  TFaceApiAsyncCallback = procedure(AResult: String) of object;

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
    class procedure DetectURL(AAccess: TAccessServer; const AURL: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback = nil);
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectFile">interface DetectFile</see>
    /// </summary>
    class procedure DetectFile(AAccess: TAccessServer; const AFileName: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback = nil);
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectStream">interface DetectStream</see>
    /// </summary>
    class procedure DetectStream(AAccess: TAccessServer; AStream: TBytesStream;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    class procedure ListPersonGroups(AAccess: TAccessServer;
      ACallbackMethod: TFaceApiAsyncCallback = nil;
      const AStart: String = ''; const ATop: Integer = CONST_COMMAND_LIST_TOP);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    class procedure ListPersonsInPersonGroup(AAccess: TAccessServer;
      const AGroupID, AStart: String; ATop: Integer;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.AddPersonFaceURL">interface AddPersonFaceURL</see>
    /// </summary>
    class procedure AddPersonFaceURL(AAccess: TAccessServer; const AGroupID, APersonID,
      AURL,  ATargetFace: String; const AUserData: String = '';
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.CreatePerson">interface CreatePerson</see>
    /// </summary>
    class procedure CreatePerson(AAccess: TAccessServer; AGroupID: String;
      APersonName: String; APersonUserData: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.GetPersonGroupTrainingStatus">interface GetPersonGroupTrainingStatus</see>
    /// </summary>
    class procedure GetPersonGroupTrainingStatus(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil);
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.TrainPersonGroup">interface TrainPersonGroup</see>
    /// </summary>
    class procedure TrainPersonGroup(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.CreatePersonGroup">interface CreatePersonGroup</see>
    /// </summary>
    class procedure CreatePersonGroup(AAccess: TAccessServer;
      const AGroupID, AGroupName, AGroupUserData: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.DeletePersonGroup">interface DeletePersonGroup</see>
    /// </summary>
    class procedure DeletePersonGroup(AAccess: TAccessServer;
      const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Verify">interface Verify (overload)</see>
    /// </summary>
    class procedure Verify(AAccess: TAccessServer;
      const AFaceTempID1, AFaceTempID2: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil); overload;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Verify">interface Verify (overload)</see>
    /// </summary>
    class procedure Verify(AAccess: TAccessServer;
      const AFaceTempID, APersonID, AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil); overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Identify">interface Identify</see>
    /// </summary>
    class procedure Identify(AAccess: TAccessServer; AFaceIDS: TStringList;
      const AGroupID: String; const AMaxNumOfCandidatesReturned: Integer = CONST_COMMAND_IDENTIFY_MaxNumOfCandidatesReturned;
			const AConfidenceThreshold: Double = CONST_Default_ConfidenceThreshold;
			ACallbackMethod: TFaceApiAsyncCallback = nil); overload;

		/// <summary>
		///   Implements asynchronous top layout for
		///   <see cref="uIFaceApi.Face|IFaceApiFace.Identify">interface Identify</see>
		///   <para>Overloaded -- same as above, but params in differet order</para>
		/// </summary>
		class procedure Identify(AAccess: TAccessServer; AFaceIDS: TStringList;
			const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback = nil;
			const AMaxNumOfCandidatesReturned: Integer = CONST_COMMAND_IDENTIFY_MaxNumOfCandidatesReturned;
			const AConfidenceThreshold: Double = CONST_Default_ConfidenceThreshold); overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.UpdatePersonGroup">interface UpdatePersonGroup</see>
    /// </summary>
    class procedure UpdatePersonGroup(AAccess: TAccessServer;
      const AGroupID: String; const AGroupName: String;
      const AGroupUserData: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.GetPersonGroup">interface GetPersonGroup</see>
    /// </summary>
    class procedure GetPersonGroup(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil);

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar (overload)</see>
    /// </summary>
    class procedure FindSimilar(AAccess: TAccessServer; const AFaceID: String;
			const AListID: String; const AMaxNumOfCandidatesReturned: Integer = CONST_COMMAND_SIMILAR_MaxNumOfCandidatesReturned;
      AFindMode: String = 'matchPerson';
      ACallbackMethod: TFaceApiAsyncCallback = nil); overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar (overload)</see>
    ///   <para>Overloaded -- same as above, but params in differet order</para>
    /// </summary>
    class procedure FindSimilar(AAccess: TAccessServer; const AFaceID: String;
			const AListID: String; ACallbackMethod: TFaceApiAsyncCallback = nil;
			const AMaxNumOfCandidatesReturned: Integer = CONST_COMMAND_SIMILAR_MaxNumOfCandidatesReturned;
			AFindMode: String = 'matchPerson'); overload;

		/// <summary>
		///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar (overload)</see>
    /// </summary>
    class procedure FindSimilar(AAccess: TAccessServer; const AFaceID: String;
			AFaceIDS: TStringList; const AMaxNumOfCandidatesReturned: Integer = CONST_COMMAND_SIMILAR_MaxNumOfCandidatesReturned;
      AFindMode: String = 'matchPerson';
      ACallbackMethod: TFaceApiAsyncCallback = nil); overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.FindSimilar">interface FindSimilar (overload)</see>
    ///   <para>Overloaded -- same as above, but params in differet order</para>
    /// </summary>
    class procedure FindSimilar(AAccess: TAccessServer; const AFaceID: String;
      AFaceIDS: TStringList; ACallbackMethod: TFaceApiAsyncCallback = nil;
			const AMaxNumOfCandidatesReturned: Integer = CONST_COMMAND_SIMILAR_MaxNumOfCandidatesReturned;
      AFindMode: String = 'matchPerson'); overload;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.Group">interface Group</see>
    /// </summary>
    class procedure Group(AAccess: TAccessServer; AFaceIDS: TStringList;
      ACallbackMethod: TFaceApiAsyncCallback = nil);
  end;

implementation

uses
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

class procedure FaceApiAsyncHelper.AddPersonFaceURL(AAccess: TAccessServer;
  const AGroupID, APersonID, AURL, ATargetFace, AUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback);
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.AddPersonFaceURL(AAccess, AGroupID, APersonID,
        AURL, ATargetFace, AUserData);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class procedure FaceApiAsyncHelper.CreatePerson(AAccess: TAccessServer; AGroupID,
  APersonName, APersonUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.CreatePersonGroup(AAccess: TAccessServer;
  const AGroupID, AGroupName, AGroupUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback = nil);
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

class procedure FaceApiAsyncHelper.DeletePersonGroup(AAccess: TAccessServer;
  const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback = nil);
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

class procedure FaceApiAsyncHelper.DetectFile(AAccess: TAccessServer;
  const AFileName: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback = nil);
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

class procedure FaceApiAsyncHelper.DetectStream(AAccess: TAccessServer;
  AStream: TBytesStream; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback = nil);
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.DetectStream(AAccess, AStream, ADetectOptions);

      // Yep, was async call, that is why we release it here
      AStream.Free;

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class procedure FaceApiAsyncHelper.DetectURL(AAccess: TAccessServer;
  const AURL: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback = nil);
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

class procedure FaceApiAsyncHelper.FindSimilar(AAccess: TAccessServer;
  const AFaceID, AListID: String; ACallbackMethod: TFaceApiAsyncCallback;
  const AMaxNumOfCandidatesReturned: Integer; AFindMode: String);
begin
  FindSimilar(AAccess, AFaceID, AListID, AMaxNumOfCandidatesReturned,
    AFindMode, ACallbackMethod);
end;

class procedure FaceApiAsyncHelper.FindSimilar(AAccess: TAccessServer;
  const AFaceID: String; AFaceIDS: TStringList;
  ACallbackMethod: TFaceApiAsyncCallback;
  const AMaxNumOfCandidatesReturned: Integer; AFindMode: String);
begin
  FindSimilar(AAccess, AFaceID, AFaceIDS, AMaxNumOfCandidatesReturned,
    AFindMode, ACallbackMethod);
end;

class procedure FaceApiAsyncHelper.FindSimilar(AAccess: TAccessServer;
  const AFaceID, AListID: String; const AMaxNumOfCandidatesReturned: Integer;
  AFindMode: String; ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.FindSimilar(AAccess: TAccessServer;
  const AFaceID: String; AFaceIDS: TStringList;
  const AMaxNumOfCandidatesReturned: Integer; AFindMode: String;
  ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.GetPersonGroup(AAccess: TAccessServer;
  const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.GetPersonGroupTrainingStatus(
  AAccess: TAccessServer; const AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.Group(AAccess: TAccessServer;
  AFaceIDS: TStringList; ACallbackMethod: TFaceApiAsyncCallback);
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.Group(AAccess, AFaceIDS);

      // Yep, was async call, that is why we release it here
      AFaceIDS.Free;

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class procedure FaceApiAsyncHelper.Identify(AAccess: TAccessServer;
  AFaceIDS: TStringList; const AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback;
  const AMaxNumOfCandidatesReturned: Integer;
  const AConfidenceThreshold: Double);
begin
  Identify(AAccess, AFaceIDS, AGroupID,
    AMaxNumOfCandidatesReturned, AConfidenceThreshold, ACallbackMethod);
end;

class procedure FaceApiAsyncHelper.Identify(AAccess: TAccessServer;
  AFaceIDS: TStringList; const AGroupID: String;
  const AMaxNumOfCandidatesReturned: Integer;
  const AConfidenceThreshold: Double;
  ACallbackMethod: TFaceApiAsyncCallback);
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.Identify(AAccess, AFaceIDS, AGroupID,
        AMaxNumOfCandidatesReturned, AConfidenceThreshold);

      // Yep, was async call, that is why we release it here
      AFaceIDS.Free;

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class procedure FaceApiAsyncHelper.ListPersonGroups(AAccess: TAccessServer;
  ACallbackMethod: TFaceApiAsyncCallback = nil;
  const AStart: String = ''; const ATop: Integer = CONST_COMMAND_LIST_TOP);
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

class procedure FaceApiAsyncHelper.ListPersonsInPersonGroup(
  AAccess: TAccessServer; const AGroupID, AStart: String; ATop: Integer;
  ACallbackMethod: TFaceApiAsyncCallback = nil);
begin
  RunAsync(
    procedure
    var
      LResult: String;
    begin
      LResult := FaceApiHelper.ListPersonsInPersonGroup(AAccess, AGroupID,
        AStart, ATop);

      CallCallback(ACallbackMethod, LResult);
    end
  );
end;

class procedure FaceApiAsyncHelper.TrainPersonGroup(AAccess: TAccessServer;
  const AGroupID: String; ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.UpdatePersonGroup(AAccess: TAccessServer;
  const AGroupID, AGroupName, AGroupUserData: String;
  ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.Verify(AAccess: TAccessServer;
  const AFaceTempID1, AFaceTempID2: String;
  ACallbackMethod: TFaceApiAsyncCallback);
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

class procedure FaceApiAsyncHelper.Verify(AAccess: TAccessServer;
  const AFaceTempID, APersonID, AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback);
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
