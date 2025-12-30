{------------------------------------------------------------------------------}
{                                                                              }
{  i18n Package                                                                }
{  Internationalization and Localization for Delphi                            }
{                                                                              }
{  Copyright (c) Kambiz Khojasteh                                              }
{  https://github.com/khojasteh/i18n                                           }
{                                                                              }
{------------------------------------------------------------------------------}

/// <summary>
/// This unit implements various hash tables to look up an object in a
/// list quickly.
/// </summary>
unit i18nHashList;

{$I DELPHIAREA.INC}

interface

uses
  SysUtils, Classes, Types, Generics.Defaults;

type

  {$region 'xmldoc'}
  /// <summary>
  /// EKeyNotFoundError is the exception class of the hash table errors.
  /// </summary>
  /// <remarks>
  /// EKeyNotFoundError occurs when the application refers to a hash key that does
  /// not exist.
  /// </remarks>
  /// <seealso cref="THashList"/>
  {$endregion}
  EKeyNotFoundError = class(Exception);

  {$region 'xmldoc'}
  /// <summary>
  /// TKeyLookup represents a hash table that associates a value to a unique key.
  /// </summary>
  /// <remarks>
  /// Use TKeyLookup when you need to locate a value using a key. Both keys and values
  /// are type generic.
  /// </remarks>
  {$endregion}
  TKeyLookup<TKey, TValue> = class(TObject)
  {$IFDEF COMPILER2010_UP}
  private
    type
      PPEntry = ^PEntry;
      PEntry = ^TEntry;
      TEntry = record
        Next: PEntry;
        Key: TKey;
        Value: TValue;
      end;
  private
    HashMask: Cardinal;
    Buckets: array of PEntry;
    IKey: IEqualityComparer<TKey>;
    function AllocateEntry(const Key: TKey; const Value: TValue): PEntry; inline;
    function ReleaseEntry(Entry: PEntry): PEntry; inline;
    function LookupBucket(const Key: TKey): PPEntry;
    function LookupEntry(const Key: TKey): PEntry; inline;
  {$ELSE}
  private
    // Delphi 2009 goes nuts if we use a pointer to a generic type. In this
    // case an internal error occurs in another unit that referencing the unit
    // of the pointer decleration.
    // Because an object is actually a pointer, we can work with generic objects
    // as they are pointers. But we will lose some performance because object
    // allocation/deallocation is slower than GetMem/Freemem approach.
    // Anyway, TKeyLookup class is still much faster than TDictionary class that
    // is declared in the Generics.Collections unit.
    type
      TEntry = class
        Next: TEntry;
        Key: TKey;
        Value: TValue;
      end;
  private
    HashMask: Cardinal;
    Buckets: array of TEntry;
    IKey: IEqualityComparer<TKey>;
    function AllocateEntry(const Key: TKey; const Value: TValue): TEntry; inline;
    function ReleaseEntry(Entry: TEntry): TEntry; inline;
    function LookupBucket(const Key: TKey): Pointer;
    function LookupEntry(const Key: TKey): TEntry; inline;
  {$ENDIF}
  public
    type
      {$region 'xmldoc'}
      /// <summary>
      /// TCallback is the type for callback handlers that respond when traversing
      /// entries of a lookup hash table.
      /// </summary>
      /// <param name="Key">
      /// The unique key of the current entry.
      /// </param>
      /// <param name="Value">
      /// The value of the current entry.
      /// </param>
      /// <param name="UserData">
      /// A user-defined value supplied as the UserData parameter of the
      /// <see cref="ForEach"/> method.
      /// </param>
      /// <returns>
      /// Returns <see langword="false"/> if there is no need to continue traversing,
      /// otherwise returns <see langword="true"/>.
      /// </returns>
      /// <seealso cref="ForEach"/>
      {$endregion}
      TCallback = reference to function(const Key: TKey; const Value: TValue): Boolean;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the lookup hash table.
    /// </summary>
    /// <param name="EqualityComparer">
    /// The interface that provides equality comparer and hasher functions for
    /// the keys.
    /// </param>
    /// <param name="Size">
    /// The bucket size of the hash table. This value should be a power of two,
    /// otherwise the constructor rounds it up to the next power of two.
    /// </param>
    {$endregion}
    constructor Create(EqualityComparer: IEqualityComparer<TKey>; Size: Cardinal = 128); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the lookup hash table.
    /// </summary>
    /// <param name="EqualityComparer">
    /// The interface that provides equality comparer and hasher functions for the
    /// keys.
    /// </param>
    /// <param name="Size">
    /// The bucket size of the hash table. This value should be a power of two,
    /// otherwise the constructor rounds it up to the next power of two.
    /// </param>
    {$endregion}
    constructor Create(Size: Cardinal = 128); overload;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the hash table and releases its allocated memory.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes all entries from the hash table.
    /// </summary>
    {$endregion}
    procedure Clear;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies entries from another hash table.
    /// </summary>
    /// <param name="Source">
    /// The source hash table.
    /// </param>
    {$endregion}
    procedure CopyFrom(Source: TKeyLookup<TKey, TValue>);
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a key-value pair to the hash table.
    /// </summary>
    /// <param name="Key">
    /// The key to add.
    /// </param>
    /// <param name="Value">
    /// The value associated with the key.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key was not already in the hash
    /// table. Otherwise, returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="AddOrUpdate"/>
    /// <seealso cref="Update"/>
    {$endregion}
    function Add(const Key: TKey; const Value: TValue): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Changes the value that is associated with a key.
    /// </summary>
    /// <param name="Key">
    /// The key to modify its associated value.
    /// </param>
    /// <param name="Value">
    /// The new value.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Add"/>
    /// <seealso cref="AddOrUpdate"/>
    {$endregion}
    function Update(const Key: TKey; const Value: TValue): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// If a specified key does not exist in the hash table, adds the key-value
    /// pair to the hash table. Otherwise, updates the value associated with the
    /// key.
    /// </summary>
    /// <param name="Key">
    /// The key to add or update.
    /// </param>
    /// <param name="Value">
    /// The value associated with the key.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if an existing entry updated. Returns
    /// <see langword="false"/> if a new entry added.
    /// </returns>
    /// <seealso cref="Add"/>
    /// <seealso cref="Update"/>
    {$endregion}
    function AddOrUpdate(const Key: TKey; const Value: TValue): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes a key and its associated value from the hash table.
    /// </summary>
    /// <param name="Key">
    /// The key to delete.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Remove(const Key: TKey): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the value that is associated with a key.
    /// </summary>
    /// <param name="Key">
    /// The key whose value to retrieve.
    /// </param>
    /// <param name="Value">
    /// The value associated with the key if the key is found.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Retrieve(const Key: TKey; out Value: TValue): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether a key is in the hash table.
    /// </summary>
    /// <param name="Key">
    /// The key to check.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function Exists(const Key: TKey): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Executes a callback for every key-value pair in the hash table.
    /// </summary>
    /// <param name="Callback">
    /// The callback to execute for each key-value pair.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if it executes the callback for every key-value
    /// pair in the hash table. Returns <see langword="false"/> if the callback returns
    /// <see langword="false"/> for some key-value pair, causing any subsequent key-value
    /// pairs to be skipped.
    /// </returns>
    {$endregion}
    function ForEach(Callback: TCallback): Boolean;
  end;

  THashList = class;

  {$region 'xmldoc'}
  /// <summary>
  /// THashItem is an individual item of a <see cref="THashList"/> class.
  /// </summary>
  /// <remarks>
  /// Use THashItem as a base class to define an object that can be maintained
  /// by an instance of <see cref="THashList"/> class.
  /// </remarks>
  /// <seealso cref="THashList"/>
  {$endregion}
  THashItem = class abstract(TPersistent)
  private
    HashList: THashList;
    HashNext: THashItem;
    OrderPrev: THashItem;
    OrderNext: THashItem;
    function GetIndex: Integer;
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the owner of the object.
    /// </summary>
    /// <returns>
    /// The owner of the object.
    /// </returns>
    {$endregion}
    function GetOwner: TPersistent; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the unique key that was passed as parameter to the constructor
    /// of the object.
    /// </summary>
    /// <returns>
    /// Returns the unique key of the object.
    /// </returns>
    /// <seealso cref="Create"/>
    {$endregion}
    function GetKey: String; virtual; abstract;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    /// <param name="AKey">
    /// The unique key of the instance that will be used to look up the object
    /// in a <see cref="THashList"/> object.
    /// </param>
    {$endregion}
    constructor Create(const AKey: String); virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Deletes the object from the owner list and destroys its instance.
    /// </summary>
    {$endregion}
    procedure Delete;
    {$region 'xmldoc'}
    /// <summary>
    /// Places this object on top of the owner list.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the object is maintained by a <see cref="THashList"/>
    /// object, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="MoveToLast"/>
    /// <seealso cref="MoveBefore"/>
    /// <seealso cref="MoveAfter"/>
    {$endregion}
    function MoveToFirst: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Places this object on bottom of the owner list.
    /// </summary>
    /// <returns>
    /// Returns <see langword="true"/> if the object is maintained by a <see cref="THashList"/>
    /// object, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="MoveToFirst"/>
    /// <seealso cref="MoveBefore"/>
    /// <seealso cref="MoveAfter"/>
    {$endregion}
    function MoveToLast: Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Places this object before another object in the owner list.
    /// </summary>
    /// <param name="Item">
    /// The object before which this object will be placed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if both objects are maintained by the same
    /// <see cref="THashList"/> object, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="MoveToFirst"/>
    /// <seealso cref="MoveToLast"/>
    /// <seealso cref="MoveAfter"/>
    {$endregion}
    function MoveBefore(Item: THashItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Places this object after another object in the owner list.
    /// </summary>
    /// <param name="Item">
    /// The object after which this object will be placed.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if both objects are maintained by the same
    /// <see cref="THashList"/> object, otherwise returns <see langword="false"/>.</returns>
    /// <seealso cref="MoveToFirst"/>
    /// <seealso cref="MoveToLast"/>
    /// <seealso cref="MoveBefore"/>
    {$endregion}
    function MoveAfter(Item: THashItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the <see cref="THashList"/> object that owns and maintains this object.
    /// </summary>
    {$endregion}
    property Owner: THashList read HashList;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner list that is placed just before this
    /// object.
    /// </summary>
    /// <seealso cref="Next"/>
    {$endregion}
    property Prev: THashItem read OrderPrev;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the object in the owner list that is placed just after this
    /// object.
    /// </summary>
    /// <seealso cref="Prev"/>
    {$endregion}
    property Next: THashItem read OrderNext;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the position of this object in the owner list.
    /// </summary>
    {$endregion}
    property Index: Integer read GetIndex;
  end;

  {$region 'xmldoc'}
  /// <summary>
  /// This is a class reference for <see cref="THashItem"/> class or for one of
  /// its descendants.
  /// </summary>
  /// <remarks>
  /// THashItemClass is the metaclass for <see cref="THashItem"/>. Its value is the
  /// class reference for <see cref="THashItem"/> or for one of its descendants.
  /// </remarks>
  {$endregion}
  THashItemClass = class of THashItem;

  {$region 'xmldoc'}
  /// <summary>
  /// THashList maintains a list of <see cref="THashItem"/> objects.
  /// </summary>
  /// <remarks>
  /// <para>
  /// THashList is combination of a double-linked list and a hash map. Therefore,
  /// it not only provides a fast sequential access to the maintained items but
  /// also can locate an individual item very quickly.
  /// </para>
  /// <para>
  /// Use THashList to store and maintain a list of <see cref="THashItem"/>
  /// objects. THashList provides properties and methods to add, delete, rearrange,
  /// locate, access, and sort the <see cref="THashItem"/> objects.
  /// </para>
  /// </remarks>
  /// <seealso cref="THashItem"/>
  {$endregion}
  THashList = class abstract(TPersistent)
  private
    Buckets: array of THashItem;
    HashMask: Cardinal;
    fCount: Integer;
    fOrderFirst: THashItem;
    fOrderLast: THashItem;
    function GetItems(const Key: String): THashItem;
    procedure TakeItemOut(Item: THashItem);
  protected
    {$region 'xmldoc'}
    /// <summary>
    /// Returns the <see cref="THashItem"/> class that represents individual
    /// items of the list.
    /// </summary>
    /// <returns>
    /// The <see cref="THashItem"/> class or one of its descendants.
    /// </returns>
    {$endregion}
    function GetItemClass: THashItemClass; virtual; abstract;
    {$region 'xmldoc'}
    /// <summary>
    /// Responds when items are added to or removed from the list.
    /// </summary>
    /// <param name="Item">
    /// The item that was just added to or that is about to be removed from the
    /// list.
    /// </param>
    /// <param name="Action">
    /// Indicates whether item was added, is about to be removed, or is about to
    /// be deleted.
    /// </param>
    {$endregion}
    procedure Notify(Item: THashItem; Action: TListNotification); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds an item in the list that is associated with a specified key.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to look up.
    /// </param>
    /// <param name="Item">
    /// The item that is found.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    {$endregion}
    function FindKey(const Key: String; out Item: THashItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Searches for a specified key in the list and if no item is associated
    /// to the key, creates the item.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to look up or create.
    /// </param>
    /// <param name="Item">
    /// The item that is found or newly created.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is not found and a new item
    /// is created for it, otherwise returns <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ExtractKey"/>
    {$endregion}
    function AddKey(const Key: String; out Item: THashItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified key from the list without destroying
    /// the item itself.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to look up and remove.
    /// </param>
    /// <param name="Item">
    /// The item that is found and removed from the list.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="ExtractKey"/>
    {$endregion}
    function ExtractKey(const Key: String; out Item: THashItem): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Relocates an item to place before another item.
    /// </summary>
    /// <param name="Item">
    /// The item to move.
    /// </param>
    /// <param name="ItemAfter">
    /// The item that will be placed after the target item.
    /// </param>
    /// <seealso cref="MoveItemAfter"/>
    {$endregion}
    procedure MoveItemBefore(Item, ItemAfter: THashItem);
    {$region 'xmldoc'}
    /// <summary>
    /// Relocates an item to place after another item.
    /// </summary>
    /// <param name="Item">
    /// The item to move.
    /// </param>
    /// <param name="ItemBefore">
    /// The item that will be placed before the target item.
    /// </param>
    /// <seealso cref="MoveItemBefore"/>
    {$endregion}
    procedure MoveItemAfter(Item, ItemBefore: THashItem);
    {$region 'xmldoc'}
    /// <summary>
    /// Calculates a 32-bit hash value of a specified key.
    /// </summary>
    /// <param name="Key">
    /// The key to calculate its hash value.
    /// </param>
    /// <returns>
    /// The 32-bit hash value of the key.
    /// </returns>
    {$endregion}
    function HashOf(const Key: String): Cardinal; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Compares two key strings in a case insensitive manner.
    /// </summary>
    /// <param name="Key1">
    /// The first key.
    /// </param>
    /// <param name="Key2">
    /// The second key.
    /// </param>
    /// <returns>
    /// Returns zero if both keys are identical. Returns -1 if <paramref name="Key1"/>
    /// is smaller than <paramref name="Key2"/>. Returns 1 if <paramref name="Key1"/>
    /// is bigger than <paramref name="Key2"/>.
    /// </returns>
    {$endregion}
    function Compare(const Key1, Key2: String): Integer; virtual;
  public
    {$region 'xmldoc'}
    /// <summary>
    /// Creates an instance of the class.
    /// </summary>
    /// <param name="Size">
    /// The bucket size of the hash table. This value should be a power of two,
    /// otherwise the constructor rounds it up to the next power of two.
    /// </param>
    {$endregion}
    constructor Create(Size: Cardinal = 256); virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys the list and all of its items.
    /// </summary>
    {$endregion}
    destructor Destroy; override;
    {$region 'xmldoc'}
    /// <summary>
    /// Destroys all the items in the list.
    /// </summary>
    {$endregion}
    procedure Clear; virtual;
    {$region 'xmldoc'}
    /// <summary>
    /// Copies items of another list to this list.
    /// </summary>
    /// <param name="Source">
    /// The source list.
    /// </param>
    {$endregion}
    procedure Assign(Source: TPersistent); override;
    {$region 'xmldoc'}
    /// <summary>
    /// Moves the item with a specified key to top of the list.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to move.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="MoveToLast"/>
    /// <seealso cref="MoveBefore"/>
    /// <seealso cref="MoveAfter"/>
    {$endregion}
    function MoveToFirst(const Key: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Moves the item with a specified key to bottom of the list.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to move.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="MoveToFirst"/>
    /// <seealso cref="MoveBefore"/>
    /// <seealso cref="MoveAfter"/>
    {$endregion}
    function MoveToLast(const Key: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Moves the item with a specified key before another item represented by
    /// its key.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to move.
    /// </param>
    /// <param name="KeyAfter">
    /// The unique key of the item that will be placed after the target item.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if both keys are found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="MoveToFirst"/>
    /// <seealso cref="MoveToLast"/>
    /// <seealso cref="MoveAfter"/>
    {$endregion}
    function MoveBefore(const Key, KeyAfter: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Moves the item with a specified key after another item represented by
    /// its key.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to move.
    /// </param>
    /// <param name="KeyBefore">
    /// The unique key of the item that will be placed before the target item.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if both keys are found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="MoveToFirst"/>
    /// <seealso cref="MoveToLast"/>
    /// <seealso cref="MoveBefore"/>
    {$endregion}
    function MoveAfter(const Key, KeyBefore: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Indicates whether an item with a specified key is in the list.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to examine.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Find"/>
    {$endregion}
    function Exists(const Key: String): Boolean; inline;
    {$region 'xmldoc'}
    /// <summary>
    /// Finds an item with a specified key in the list.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to find.
    /// </param>
    /// <returns>
    /// The found item or <see langword="nil"/> if the key is not found.
    /// </returns>
    /// <seealso cref="Exists"/>
    {$endregion}
    function Find(const Key: String): THashItem;
    {$region 'xmldoc'}
    /// <summary>
    /// Adds a new item if a specified key is not already in the list.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the new item.
    /// </param>
    /// <returns>
    /// The item with the specified key.
    /// </returns>
    {$endregion}
    function Add(const Key: String): THashItem;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified key from the list and destroys its
    /// object.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to delete.
    /// </param>
    /// <returns>
    /// Returns <see langword="true"/> if the key is found, otherwise returns
    /// <see langword="false"/>.
    /// </returns>
    /// <seealso cref="Extract"/>
    {$endregion}
    function Delete(const Key: String): Boolean;
    {$region 'xmldoc'}
    /// <summary>
    /// Removes an item with a specified key from the list without destroying
    /// its object.
    /// </summary>
    /// <param name="Key">
    /// The unique key of the item to extract.
    /// </param>
    /// <returns>
    /// The item that is removed from the list, or <see langword="nil"/> if the
    /// key is not found.
    /// </returns>
    /// <seealso cref="Delete"/>
    {$endregion}
    function Extract(const Key: String): THashItem;
    {$region 'xmldoc'}
    /// <summary>
    /// Sorts the list's items in alphabetical order by their keys.
    /// </summary>
    {$endregion}
    procedure Sort;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the number of items in the list.
    /// </summary>
    {$endregion}
    property Count: Integer read fCount;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the item on top of the list.
    /// </summary>
    {$endregion}
    property First: THashItem read fOrderFirst;
    {$region 'xmldoc'}
    /// <summary>
    /// Gets the item on bottom of the list.
    /// </summary>
    {$endregion}
    property Last: THashItem read fOrderLast;
    {$region 'xmldoc'}
    /// <summary>
    /// Lists the items in the list by their unique keys.
    /// </summary>
    /// <exception cref="EKeyNotFoundError">
    /// Occurs when a key is not in the list.
    /// </exception>
    {$endregion}
    property Items[const Key: String]: THashItem read GetItems; default;
  end;

implementation

resourcestring
  SKeyNotFoundError = 'Key ''%s'' is not found';

{ TKeyLookup<TKey, TValue> }

constructor TKeyLookup<TKey, TValue>.Create(EqualityComparer: IEqualityComparer<TKey>;
  Size: Cardinal);
var
  BucketSize: Cardinal;
begin
  IKey := EqualityComparer;
  BucketSize := 4;
  while BucketSize < Size do
    BucketSize := BucketSize shl 1;
  SetLength(Buckets, BucketSize);
  HashMask := BucketSize - 1;
end;

constructor TKeyLookup<TKey, TValue>.Create(Size: Cardinal);
begin
  Create(TEqualityComparer<TKey>.Default, Size);
end;

destructor TKeyLookup<TKey, TValue>.Destroy;
begin
  Clear;
  SetLength(Buckets, 0);
  inherited Destroy;
end;

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.AllocateEntry(const Key: TKey; const Value: TValue): PEntry;
begin
  // We could use New(Result) but because of Delphi compiler bug, the New
  // standard procedure fails to initialize generic records. The folliwng
  /// three lines are a workaround for this problem.
  GetMem(Result, SizeOf(TEntry));
  Initialize(Result^.Key);    // Key can be long string, variant, or interface
  Initialize(Result^.Value);  // Value can be long string, variant, or interface
  // initialize the new allocated record
  Result^.Key := Key;
  Result^.Value := Value;
  Result^.Next := nil;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.AllocateEntry(const Key: TKey; const Value: TValue): TEntry;
begin
  Result := TEntry.Create;
  Result.Key := Key;
  Result.Value := Value;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.ReleaseEntry(Entry: PEntry): PEntry;
begin
  Result := Entry^.Next;
  Finalize(Entry^.Key);       // Key can be long string, variant, or interface
  Finalize(Entry^.Value);     // Value can be long string, variant, or interface
  FreeMem(Entry);
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.ReleaseEntry(Entry: TEntry): TEntry;
begin
  Result := Entry.Next;
  Entry.Free;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.LookupBucket(const Key: TKey): PPEntry;
begin
  Result := @Buckets[IKey.GetHashCode(Key) and HashMask];
  while (Result^ <> nil) and not IKey.Equals(Result^^.Key, Key) do
    Result := @(Result^^.Next);
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.LookupBucket(const Key: TKey): Pointer;
begin
  Result := @Buckets[IKey.GetHashCode(Key) and HashMask];
  while (TEntry(Result^) <> nil) and not IKey.Equals(TEntry(Result^).Key, Key) do
    Result := @(TEntry(Result^).Next);
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.LookupEntry(const Key: TKey): PEntry;
begin
  Result := LookupBucket(Key)^;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.LookupEntry(const Key: TKey): TEntry;
begin
  Result := TEntry(LookupBucket(Key)^);
end;
{$ENDIF}

procedure TKeyLookup<TKey, TValue>.Clear;
var
  I: Integer;
  {$IFDEF COMPILER2010_UP}
  Entry: PEntry;
  {$ELSE}
  Entry: TEntry;
  {$ENDIF}
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    Entry := Buckets[I];
    while Entry <> nil do
      Entry := ReleaseEntry(Entry);
    Buckets[I] := nil;
  end;
end;

procedure TKeyLookup<TKey, TValue>.CopyFrom(Source: TKeyLookup<TKey, TValue>);
begin
  Clear;
  Source.ForEach(
    function(const Key: TKey; const Value: TValue): Boolean
    begin
      Add(Key, Value);
      Result := True;
    end);
end;

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.Add(const Key: TKey; const Value: TValue): Boolean;
var
  Prev: PPEntry;
  Entry: PEntry;
begin
  Prev := LookupBucket(Key);
  Entry := Prev^;
  if Entry = nil then
  begin
    Entry := AllocateEntry(Key, Value);
    Prev^ := Entry;
    Result := True;
  end
  else
    Result := False;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.Add(const Key: TKey; const Value: TValue): Boolean;
var
  Prev: Pointer;
  Entry: TEntry;
begin
  Prev := LookupBucket(Key);
  Entry := TEntry(Prev^);
  if Entry = nil then
  begin
    Entry := AllocateEntry(Key, Value);
    TEntry(Prev^) := Entry;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.Update(const Key: TKey; const Value: TValue): Boolean;
var
  Entry: PEntry;
begin
  Entry := LookupEntry(Key);
  if Entry <> nil then
  begin
    Entry^.Value := Value;
    Result := True;
  end
  else
    Result := False;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.Update(const Key: TKey; const Value: TValue): Boolean;
var
  Entry: TEntry;
begin
  Entry := LookupEntry(Key);
  if Entry <> nil then
  begin
    Entry.Value := Value;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.AddOrUpdate(const Key: TKey; const Value: TValue): Boolean;
var
  Prev: PPEntry;
  Entry: PEntry;
begin
  Prev := LookupBucket(Key);
  Entry := Prev^;
  if Entry <> nil then
  begin
    Entry^.Value := Value;
    Result := True;
  end
  else
  begin
    Entry := AllocateEntry(Key, Value);
    Prev^ := Entry;
    Result := False;
  end;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.AddOrUpdate(const Key: TKey; const Value: TValue): Boolean;
var
  Prev: Pointer;
  Entry: TEntry;
begin
  Prev := LookupBucket(Key);
  Entry := TEntry(Prev^);
  if Entry <> nil then
  begin
    Entry.Value := Value;
    Result := True;
  end
  else
  begin
    Entry := TEntry.Create;
    Entry.Key := Key;
    Entry.Value := Value;
    TEntry(Prev^) := Entry;
    Result := False;
  end;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.Remove(const Key: TKey): Boolean;
var
  Entry: PEntry;
  Prev: PPEntry;
begin
  Prev := LookupBucket(Key);
  Entry := Prev^;
  if Entry <> nil then
  begin
    Prev^ := ReleaseEntry(Entry);
    Result := True;
  end
  else
    Result := False;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.Remove(const Key: TKey): Boolean;
var
  Prev: Pointer;
  Entry: TEntry;
begin
  Prev := LookupBucket(Key);
  Entry := TEntry(Prev^);
  if Entry <> nil then
  begin
    TENtry(Prev^) := Entry.Next;
    Entry.Free;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF}

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.Retrieve(const Key: TKey; out Value: TValue): Boolean;
var
  Entry: PEntry;
begin
  Entry := LookupEntry(Key);
  if Entry <> nil then
  begin
    Value := Entry^.Value;
    Result := True;
  end
  else
    Result := False;
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.Retrieve(const Key: TKey; out Value: TValue): Boolean;
var
  Entry: TEntry;
begin
  Entry := LookupEntry(Key);
  if Entry <> nil then
  begin
    Value := Entry.Value;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF}

function TKeyLookup<TKey, TValue>.Exists(const Key: TKey): Boolean;
begin
  Result := (LookupEntry(Key) <> nil);
end;

{$IFDEF COMPILER2010_UP}
function TKeyLookup<TKey, TValue>.ForEach(Callback: TCallback): Boolean;
var
  I: Integer;
  Entry: PEntry;
begin
  Entry := nil;
  if Assigned(Callback) then
  begin
    for I := 0 to Length(Buckets) - 1 do
    begin
      Entry := Buckets[I];
      while (Entry <> nil) and Callback(Entry^.Key, Entry^.Value) do
        Entry := Entry^.Next;
    end;
  end;
  Result := (Entry = nil);
end;
{$ELSE}
function TKeyLookup<TKey, TValue>.ForEach(Callback: TCallback): Boolean;
var
  I: Integer;
  Entry: TEntry;
begin
  Entry := nil;
  if Assigned(Callback) then
  begin
    for I := 0 to Length(Buckets) - 1 do
    begin
      Entry := Buckets[I];
      while (Entry <> nil) and Callback(Entry.Key, Entry.Value) do
        Entry := Entry.Next;
    end;
  end;
  Result := (Entry = nil);
end;
{$ENDIF}

{ THashItem }

function THashItem.GetIndex: Integer;
var
  Item: THashItem;
begin
  if HashList <> nil then
  begin
    Result := 0;
    Item := HashList.First;
    while Item <> nil do
    begin
      if Item = Self then
        Exit;
      Inc(Result);
      Item := Item.Next;
    end;
  end;
  Result := -1;
end;

function THashItem.GetOwner: TPersistent;
begin
  Result := HashList;
end;

function THashItem.MoveToFirst: Boolean;
begin
  Result := False;
  if HashList <> nil then
  begin
    HashList.MoveItemBefore(Self, HashList.First);
    Result := True;
  end;
end;

function THashItem.MoveToLast: Boolean;
begin
  Result := False;
  if HashList <> nil then
  begin
    HashList.MoveItemAfter(Self, HashList.Last);
    Result := True;
  end;
end;

function THashItem.MoveAfter(Item: THashItem): Boolean;
begin
  Result := False;
  if (HashList <> nil) and (HashList = Item.HashList) then
  begin
    HashList.MoveItemAfter(Self, Item);
    Result := True;
  end;
end;

function THashItem.MoveBefore(Item: THashItem): Boolean;
begin
  Result := False;
  if (HashList <> nil) and (HashList = Item.HashList) then
  begin
    HashList.MoveItemBefore(Self, Item);
    Result := True;
  end;
end;

procedure THashItem.Delete;
begin
  if HashList <> nil then
    HashList.Delete(GetKey)
  else
    Free;
end;

{ THashList }

constructor THashList.Create(Size: Cardinal);
var
  BucketSize: Cardinal;
begin
  BucketSize := 4;
  while BucketSize < Size do
    BucketSize := BucketSize shl 1;
  SetLength(Buckets, BucketSize);
  HashMask := BucketSize - 1;
end;

destructor THashList.Destroy;
begin
  Clear;
  SetLength(Buckets, 0);
  inherited Destroy;
end;

function THashList.GetItems(const Key: String): THashItem;
begin
  if not FindKey(Key, Result) then
    raise EKeyNotFoundError.CreateResFmt(@SKeyNotFoundError, [Key]);
end;

procedure THashList.Notify(Item: THashItem; Action: TListNotification);
begin
end;

function THashList.HashOf(const Key: String): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor Ord(UpCase(Key[I]));
end;

function THashList.Compare(const Key1, Key2: String): Integer;
begin
  Result := CompareText(Key1, Key2);
end;

function THashList.FindKey(const Key: String; out Item: THashItem): Boolean;
var
  C: Integer;
begin
  Result := False;
  Item := Buckets[HashOf(Key) and HashMask];
  while Item <> nil do
  begin
    C := Compare(Item.GetKey, Key);
    if C < 0 then
      Item := Item.HashNext
    else
    begin
      Result := (C = 0);
      Exit;
    end;
  end;
end;

function THashList.AddKey(const Key: String; out Item: THashItem): Boolean;
var
  pNextItem: ^THashItem;
  NextItem: THashItem;
  C: Integer;
begin
  pNextItem := @Buckets[HashOf(Key) and HashMask];
  NextItem := pNextItem^;
  while NextItem <> nil do
  begin
    C := Compare(NextItem.GetKey, Key);
    if C < 0 then
    begin
      pNextItem := @NextItem.HashNext;
      NextItem := NextItem.HashNext;
    end
    else if C = 0 then
    begin
      Item := NextItem;
      Result := False;
      Exit;
    end
    else
      Break;
  end;
  Item := GetItemClass.Create(Key);
  pNextItem^ := Item;
  Item.HashList := Self;
  Item.HashNext := NextItem;
  if fOrderFirst = nil then
    fOrderFirst := Item;
  if fOrderLast <> nil then
    fOrderLast.OrderNext := Item;
  Item.OrderPrev := fOrderLast;
  Item.OrderNext := nil;
  fOrderLast := Item;
  Inc(fCount);
  Result := True;
end;

function THashList.ExtractKey(const Key: String; out Item: THashItem): Boolean;
var
  pNextItem: ^THashItem;
  C: Integer;
begin
  Result := False;
  pNextItem := @Buckets[HashOf(Key) and HashMask];
  Item := pNextItem^;
  while Item <> nil do
  begin
    C := Compare(Item.GetKey, Key);
    if C < 0 then
    begin
      pNextItem := @Item.HashNext;
      Item := Item.HashNext;
    end
    else
    begin
      if C = 0 then
      begin
        TakeItemOut(Item);
        pNextItem^ := Item.HashNext;
        Item.HashList := nil;
        Item.HashNext := nil;
        Item.OrderPrev := nil;
        Item.OrderNext := nil;
        Dec(fCount);
        Result := True;
      end;
      Exit;
    end;
  end;
end;

procedure THashList.TakeItemOut(Item: THashItem);
begin
  if Item.OrderPrev <> nil then
    Item.OrderPrev.OrderNext := Item.OrderNext
  else
    fOrderFirst := Item.OrderNext;
  if Item.OrderNext <> nil then
    Item.OrderNext.OrderPrev := Item.OrderPrev
  else
    fOrderLast := Item.OrderPrev;
end;

procedure THashList.MoveItemBefore(Item, ItemAfter: THashItem);
begin
  if Item.OrderNext <> ItemAfter then
  begin
    TakeItemOut(Item);
    if ItemAfter.OrderPrev <> nil then
      ItemAfter.OrderPrev.OrderNext := Item
    else
      fOrderFirst := Item;
    Item.OrderPrev := ItemAfter.OrderPrev;
    Item.OrderNext := ItemAfter;
    ItemAfter.OrderPrev := Item;
  end;
end;

procedure THashList.MoveItemAfter(Item, ItemBefore: THashItem);
begin
  if Item.OrderPrev <> ItemBefore then
  begin
    TakeItemOut(Item);
    if ItemBefore.OrderNext <> nil then
      ItemBefore.OrderNext.OrderPrev := Item
    else
      fOrderLast := Item;
    Item.OrderNext := ItemBefore.OrderNext;
    Item.OrderPrev := ItemBefore;
    ItemBefore.OrderNext := Item;
  end;
end;

function THashList.MoveToFirst(const Key: String): Boolean;
var
  Item: THashItem;
begin
  if fOrderFirst <> nil then
    if Compare(fOrderFirst.GetKey, Key) <> 0 then
      if FindKey(Key, Item) then
      begin
        MoveItemBefore(Item, fOrderFirst);
        Result := True;
      end
      else
        Result := False
    else
      Result := True
  else
    Result := False;
end;

function THashList.MoveToLast(const Key: String): Boolean;
var
  Item: THashItem;
begin
  if fOrderLast <> nil then
    if Compare(fOrderLast.GetKey, Key) <> 0 then
      if FindKey(Key, Item) then
      begin
        MoveItemBefore(Item, fOrderLast);
        Result := True;
      end
      else
        Result := False
    else
      Result := True
  else
    Result := False;
end;

function THashList.MoveBefore(const Key, KeyAfter: String): Boolean;
var
  Item, ItemAfter: THashItem;
begin
  Result := False;
  if FindKey(Key, Item) and FindKey(KeyAfter, ItemAfter) then
  begin
    MoveItemBefore(Item, ItemAfter);
    Result := True;
  end;
end;

function THashList.MoveAfter(const Key, KeyBefore: String): Boolean;
var
  Item, ItemBefore: THashItem;
begin
  Result := False;
  if FindKey(Key, Item) and FindKey(KeyBefore, ItemBefore) then
  begin
    MoveItemAfter(Item, ItemBefore);
    Result := True;
  end;
end;

function THashList.Exists(const Key: String): Boolean;
var
  Item: THashItem;
begin
  Result := FindKey(Key, Item);
end;

function THashList.Find(const Key: String): THashItem;
begin
  if not FindKey(Key, Result) then
    Result := nil;
end;

function THashList.Add(const Key: String): THashItem;
begin
  if AddKey(Key, Result) then
    Notify(Result, lnAdded);
end;

function THashList.Delete(const Key: String): Boolean;
var
  Item: THashItem;
begin
  Result := False;
  if ExtractKey(Key, Item) then
  begin
    Notify(Item, lnDeleted);
    Item.Free;
    Result := True;
  end;
end;

function THashList.Extract(const Key: String): THashItem;
begin
  if ExtractKey(Key, Result) then
    Notify(Result, lnExtracted)
  else
    Result := nil;
end;

procedure THashList.Clear;
var
  Item: THashItem;
  Kill: THashItem;
begin
  if fOrderFirst <> nil then
  begin
    Item := fOrderFirst;
    while Item <> nil do
    begin
      Buckets[HashOf(Item.GetKey) and HashMask] := nil;
      Kill := Item;
      Item := Item.OrderNext;
      Notify(Kill, lnDeleted);
      Kill.Free;
    end;
    fOrderFirst := nil;
    fOrderLast := nil;
    fCount := 0;
  end;
end;

procedure THashList.Assign(Source: TPersistent);
var
  SrcItem: THashItem;
begin
  if (Source is THashList) and (GetItemClass = THashList(Source).GetItemClass) then
  begin
    Clear;
    SrcItem := THashList(Source).First;
    while SrcItem <> nil do
    begin
      Add(SrcItem.GetKey).Assign(SrcItem);
      SrcItem := SrcItem.Next;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure THashList.Sort;
var
  Item: THashItem;
  NextItem: THashItem;
  BeforeItem: THashItem;
begin
  if fOrderFirst <> nil then
  begin
    Item := fOrderFirst.OrderNext;
    while Item <> nil do
    begin
      NextItem := Item.Next;
      BeforeItem := fOrderFirst;
      while BeforeItem <> Item do
      begin
        if Compare(Item.GetKey, BeforeItem.GetKey) < 0 then
        begin
          MoveItemBefore(Item, BeforeItem);
          Break;
        end;
        BeforeItem := BeforeItem.Next;
      end;
      Item := NextItem;
    end;
  end;
end;

end.



