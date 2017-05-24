unit uFunctions.FaceApiAsyncHelper;

interface

uses
  { TDetectOptions }
  uFaceApi.FaceDetectOptions,
  { TBytesStream }
  System.Classes,
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  TFaceApiAsyncCallback = procedure(const AResult: String) of object;

  FaceApiAsyncHelper = class
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectURL">interface DetectURL</see>
    /// </summary>
    class function DetectURL(AAccess: TAccessServer; const AURL: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback): String;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectFile">interface DetectFile</see>
    /// </summary>
    class function DetectFile(AAccess: TAccessServer; const AFileName: String;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback): String;
    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.Face|IFaceApiFace.DetectStream">interface DetectStream</see>
    /// </summary>
    class function DetectStream(AAccess: TAccessServer; AStream: TBytesStream;
      const ADetectOptions: TDetectOptions;
      ACallbackMethod: TFaceApiAsyncCallback): String;

    /// <summary>
    ///   Implements asynchronous top layout for
    ///   <see cref="uIFaceApi.PersonGroup|IFaceApiPersonGroup.ListPersonGroups">interface ListPersonGroups</see>
    /// </summary>
    class function ListPersonGroups(AAccess: TAccessServer;
      const AStart: String = ''; const ATop: Integer = 1000;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;

    /// <summary>
    ///   Implements top layout for
    ///   <see cref="uIFaceApi.Core|IFaceApiCore.ListPersonsInPersonGroup">interface ListPersonsInPersonGroup</see>
    /// </summary>
    class function ListPersonsInPersonGroup(AAccess: TAccessServer;
      const AGroupID: String;
      ACallbackMethod: TFaceApiAsyncCallback = nil): String;
  end;

implementation

uses
  { TTask.Create }
  System.SysUtils,
  { IFaceApiCore }
  uIFaceApi.Core,
  { TFaceApiCore }
  uFaceApi.Core,
  { IFaceApiFace }
  uIFaceApi.Face,
  { TFaceApiCoreFace }
//  uFaceApi.Core.Face,
  { IFaceApiPersonGroup }
//  uIFaceApi.PersonGroup,
  { ITask }
  System.Threading,
  { TFaceApiCorePersonGroup }
//  uFaceApi.Core.PersonGroup,
  { FaceApiHelper }
  uFunctions.FaceApiHelper;

procedure RunAsync(AProc: TProc);
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

class function FaceApiAsyncHelper.DetectFile(AAccess: TAccessServer;
  const AFileName: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback): String;
var
  LResult: String;
begin
  RunAsync(
    procedure
    begin
      LResult := FaceApiHelper.DetectFile(AAccess, AFileName, ADetectOptions);

      if Assigned(ACallbackMethod) then
        ACallbackMethod(LResult);
    end
  );
end;

class function FaceApiAsyncHelper.DetectStream(AAccess: TAccessServer;
  AStream: TBytesStream; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback): String;
var
  LResult: String;
begin
  RunAsync(
    procedure
    begin
      LResult := FaceApiHelper.DetectStream(AAccess, AStream, ADetectOptions);

      if Assigned(ACallbackMethod) then
        ACallbackMethod(LResult);
    end
  );
end;

class function FaceApiAsyncHelper.DetectURL(AAccess: TAccessServer;
  const AURL: String; const ADetectOptions: TDetectOptions;
  ACallbackMethod: TFaceApiAsyncCallback): String;
var
  LResult: String;
begin
  RunAsync(
    procedure
    begin
      LResult := FaceApiHelper.DetectURL(AAccess, AURL, ADetectOptions);

      if Assigned(ACallbackMethod) then
        ACallbackMethod(LResult);
    end
  );
end;

class function FaceApiAsyncHelper.ListPersonGroups(AAccess: TAccessServer;
  const AStart: String; const ATop: Integer;
  ACallbackMethod: TFaceApiAsyncCallback): String;
var
  LResult: String;
begin
  RunAsync(
    procedure
    begin
      LResult := FaceApiHelper.ListPersonGroups(AAccess, AStart, ATop);

      if Assigned(ACallbackMethod) then
        ACallbackMethod(LResult);
    end
  );
end;

class function FaceApiAsyncHelper.ListPersonsInPersonGroup(
  AAccess: TAccessServer; const AGroupID: String;
  ACallbackMethod: TFaceApiAsyncCallback): String;
var
  LResult: String;
begin
  RunAsync(
    procedure
    begin
      LResult := FaceApiHelper.ListPersonsInPersonGroup(AAccess, AGroupID);

      if Assigned(ACallbackMethod) then
        ACallbackMethod(LResult);
    end
  );
end;

end.
