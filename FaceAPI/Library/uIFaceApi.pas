/// <summary>
///   Unit contain the Main Interface declaration for Face API Microsoft Cognitive Services 1.0
/// </summary>
unit uIFaceApi;

interface

uses
  { TAccess }
  uFaceApi.ServersAccess.Types;

type
  /// <summary>
  ///   Main Interface for Face API Microsoft Cognitive Services 1.0
  /// </summary>
  IFaceApi = interface(IInterface)
    ['{904FC5EC-6ECC-49EB-A3B0-7D785A5D23D2}']

    /// <summary>
    ///   List all persons in a person group, and retrieve person information (including personId, name, userData and persistedFaceIds of registered faces of the person).
    /// </summary>
		/// <param name="AGroupID">
    ///   Id of the target person group.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an array of person information that belong to the person group.
    ///   JSON fields in response body:
    ///   Fields	          Type	Description
    ///   personId	        String	personId of the person in the person group.
    ///   name	            String	Person's display name.
    ///   userData	        String	User-provided data attached to the person.
    ///   persistedFaceIds  Array	  persistedFaceId array of registered faces of the person.
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function ListPersonsInPersonGroup(const AGroupID: String): String;

    /// <summary>
    ///   Create a new person in a specified person group. A newly created person have no registered face.
    ///   The number of persons has a subscription level limit and person group level limit. Person group level limit is 1000 for both free and paid tier subscriptions.
    ///   Subscription level limit is 1000 for free tier subscription and can be greater in paid tier subscriptions.
    /// </summary>
		/// <param name="AGroupID">
    ///   Specifying the target person group to create the person.
		/// </param>
		/// <param name="APersonName">
    ///   Display name of the target person. The maximum length is 128.
		/// </param>
		/// <param name="APersonUserData">
    ///   (optional)
    ///   Optional fields for user-provided data attached to a person. Size limit is 16KB.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns a new personId created.
    ///   JSON fields in response body:
    ///   Fields	  Type	  Description
    ///   personId	String	personID of the new created person.
    ///
    ///   [ERROR]
    ///   Response 400, 401, 403, 404, 409, 415, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function CreatePerson(const AGroupID, APersonName: String; const APersonUserData: String = ''): String;

    /// <summary>
    ///   Delete an existing person from a person group. Persisted face images of the person will also be deleted.
    /// </summary>
		/// <param name="AGroupID">
    ///   Specifying the person group containing the person.
		/// </param>
		/// <param name="APersonID">
    ///   The target personId to delete.
		/// </param>
    /// <returns>
    ///   [OK]
    ///   Response 200
    ///   A successful call returns an empty response body.
    ///
    ///   [ERROR]
    ///   Response 401, 403, 404, 409, 429
    ///   Error code and message returned in JSON:
    ///   Error Code	Error Message Description
    /// </returns>
    function DeletePerson(const AGroupID, APersonID: String): String;

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
