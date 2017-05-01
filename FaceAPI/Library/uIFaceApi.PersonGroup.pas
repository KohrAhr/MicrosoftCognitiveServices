/// <summary>
///   Unit contain the Main Interface declaration for "Person Group" functionality for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uIFaceApi.PersonGroup;

interface

uses
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  /// <summary>
  ///   Main Interface for "Person Group" functionality for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  IFaceApiPersonGroup = interface(IInterface)
    ['{4837303E-2840-4C74-B3FA-E468F6564D36}']

    /// <summary>
    ///   List person groups and their information.
    /// </summary>
		/// <param name="AStart">
		///   (optional)
    ///   List person groups from the least personGroupId greater than the "start". It contains no more than 64 characters. Default is empty.
		/// </param>
		/// <param name="ATop">
		///   (optional)
    ///   The number of person groups to list, ranging in [1, 1000]. Default is 1000.
		/// </param>
		/// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an array of person groups and their information (personGroupId, name and userData).
    ///   JSON fields in response body:
    ///   Fields	Type	Description
    ///   personGroupId	  String	  personGroupId of the existing person groups, created in Person Group - Create a Person Group.
    ///   name	          String	  Person group's display name.
    ///   userData	      String	  User-provided data attached to this person group.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code  Error Message Description
    /// </returns>
    function ListPersonGroups(const AStart: String = ''; const ATop: Integer = 1000): String;

    /// <summary>
    ///   Retrieve the training status of a person group (completed or ongoing).
    ///   The training will process for a while on the server side.
    /// </summary>
		/// <param name="AGroupID">
    ///   String
    ///   Id of target person group.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns the person group's training status.
    ///   JSON fields in response body:
    ///   Fields	            Type	  Description
    ///   status	            String	Training status: notstarted, running, succeeded, failed. If the training process is waiting to perform, the status is notstarted. If the training is ongoing, the status is running. Status succeed means this person group is ready for Face - Identify. Status failed is often caused by no person or no persisted face exist in the person group.
    ///   createdDateTime	    String	A combined UTC date and time string that describes person group created time, delimited by the letter T. E.g. 1/3/2017 4:11:35 AM.
    ///   lastActionDateTime	String	Person group last modify time in the UTC, could be null value when the person group is not successfully trained.
    ///   message	            String	Show failure message when training failed (omitted when training succeed).
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function GetPersonGroupTrainingStatus(const AGroupID: String): String;
    /// <summary>
    ///   Queue a person group training task, the training task may not be started immediately.
    ///   Any updates to person group will not take effect in Face - Identify until person group is successfully trained.
    /// </summary>
		/// <param name="AGroupID">
    ///   String
    ///   Id of the Target person group to be trained.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 202
    ///   A successful call returns an empty JSON body.
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function TrainPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Create a new person group with specified personGroupId, name and user-provided userData.
    ///   A person group is one of the most important parameters for the Face - Identify API.
    /// </summary>
    /// <param name="AGroupID">
    ///   User-provided personGroupId as a string. The valid characters include numbers, English letters in lower case, '-' and '_'. The size limit is 64.
		/// </param>
    /// <param name="AGroupName">
    ///   Person group display name. The maximum length is 128.
		/// </param>
    /// <param name="AGroupUserData">
    ///   User-provided data attached to the person group. The size limit is 16KB.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an empty response body.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function CreatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;

    /// <summary>
    ///   Delete an existing person group. Persisted face images of all people in the person group will also be deleted.
    /// </summary>
		/// <param name="AGroupID">
    ///  The personGroupId of the person group to be deleted.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an empty response body.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function DeletePersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Update an existing person group's display name and userData.
    /// </summary>
    /// <param name="AGroupID">
    ///   Id of the person group to be updated
		/// </param>
    /// <param name="AGroupName">
    ///   Person group display name. The maximum length is 128.
		/// </param>
    /// <param name="AGroupUserData">
    ///   User-provided data attached to the person group. The size limit is 16KB.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an empty response body.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function UpdatePersonGroup(const AGroupID: String; const AGroupName: String; const AGroupUserData: String): String;

    /// <summary>
    ///   Retrieve the information of a person group, including its name and userData. This API returns person group information only, use Person - List Persons in a Person Group instead to retrieve person information under the person group.
    /// </summary>
    /// <param name="AGroupID">
    ///   Id of the target person group
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns the person group's information.
    ///   JSON fields in response body:
    ///   Fields	      Type	  Description
    ///   personGroupId	String	Target personGroupId provided in request parameter.
    ///   name	        String	Person group's display name.
    ///   userData	    String	User-provided data attached to this person group.
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function GetPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Define your access key and define your server location
    /// </summary>
		/// <param name="AAccess">
    ///   - Subscription key which provides access to this API. Found in your Cognitive Services accounts.
    ///   - Cognitive Api server location
		/// </param>
    procedure SetAccessKey(const AAccess: TAccess);
  end;

implementation

end.