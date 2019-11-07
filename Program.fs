// Julian Tran - 014425370
// 11/6/19
// CECS 424 - Lab 3

type RegistrationAttempt = {username: string; email: string}

// A better return type:
type ValidationResult =
    | Success of RegistrationAttempt
    | Failure of string

// A function can now return a ValidationResult, indicating success or failure. We can also
// start composing our validation functions.
let usernameExists reg =
    if reg.username.Length > 0 then
        Success reg
    else
        Failure "Username must not be blank"
// username: RegistrationAttempt -> ValidationResult

let emailHasAtSign reg = 
    if reg.email.Contains("@") then
        Success reg
    else
        Failure "Email address must contain a @"

let emailHasLocalPart reg =
    if reg.email.IndexOf("@") > 0 then
        Success reg
    else
        Failure "Email address does not have a local-part"

let bind switchFunction twoTrackInput =
    match twoTrackInput with
    | Success s -> switchFunction s
    | Failure f -> Failure f

// Binding is so common that we can introduce an operator to make our life easier.
let (>=>) switch1 switch2 reg =
    bind switch2 (switch1 reg)

// **CODE ABOVE GIVEN FROM LECTURE**   

//1) Create a list of strings named existingAccounts containing five distinct (but fake) email addresses, 
//none of which contains a period or dash.
let existingAccounts = 
    ["juliantran@gmail.com" ; "fsharpiscool@yahoo.com" ; "ilovececs424@gmail.com" ; 
        "ineedmoney@aol.com" ; "lifeishard@yahoo.com"]

//2) Create another list of strings named blacklistedDomains containing the values "mailinator.org"
//and "throwawaymail.com".
let blacklistedDomains = 
    ["mailinator.org" ; "throwawaymail.com"]

//3) a. A terminal function uniqueEmail, which takes a list of strings and a RegistrationAttempt as
//      parameters, and validates that the attempt's email address is not in the list of strings. You cannot
//      access the global list you created in step (1); you must use the parameter to the function.

let uniqueEmail listOfEmails reg =
    if (List.contains reg.email listOfEmails) then
        Failure "Email already exists"
    else 
        Success reg

//  b. A terminal function emailNotBlacklisted, which takes a list of strings and a RegistrationAttempt
//     and validates that the domain of the email address (following the @) is not in the list of strings.

let getDomain reg =
    let i = reg.email.IndexOf("@")
    reg.email.[i+1..] //stuff after @ and on

let emailNotBlacklisted domains reg =
    let emailDomain = getDomain reg
    if (List.contains emailDomain domains) then
        Failure "Domain is blacklisted"
    else
        Success reg

//4) a. Write the function bypass, which takes a single-track function and a RegistrationAttempt as
//      parameters, invokes the single-track function on the RegistrationAttempt, and returns the result
//      as a Success.

let bypass singleTrack reg = 
    Success (singleTrack reg)

//   b. Write an operator >-> by mimicking the >=> operator, except that:
//      i. the second parameter is not a switch function, but a bypassFunction.
//      ii. the bypassFunction needs to be promoted to switch using bypass before passing it to bind.
//      Everything else in the operator stays the same.

let (>->) switch1 bypassFunction reg =
    bind (bypass bypassFunction) (switch1 reg)

//5) a. lowercaseEmail, which takes a RegistrationAttempt and returns a new RegistrationAttempt
//      with the same username and the email address converted to all-lowercase.

let lowercaseEmail reg = 
    let allLowercase = reg.email.ToLower()
    {username = reg.username; email = allLowercase}

//   b. canonicalizeEmail, which takes a RegistrationAttempt and canonicalizes the email address
//      of the attempt only if the domain of the email (to the right of the @) is gmail.com. 
let getLocalPart reg = 
    let j = reg.email.IndexOf("@")
    reg.email.[..j-1] //stuff to left of @

let remove char =
    String.collect (fun x -> if Seq.exists((=)x) char then "" else string x)

let canonicalizeEmail reg = 
    let emailDomain = getDomain reg

    // remove all periods and dashes in the local-part (the left of the @ symbol). 
    if emailDomain = "gmail.com" then
        let emailLocalPart = getLocalPart reg
        
        let removeSymbol = remove ".-" (emailLocalPart)

        //if a + is present in the local-part, remove it and all characters that follow it in the local part.
        let removePlus = removeSymbol.IndexOf("+")
        if removePlus > 0 then 
            let removeAfter = removeSymbol.[..removePlus-1]
            {username = reg.username ; email = removeAfter + "@" + emailDomain}
        else 
            {username = reg.username ; email = removeSymbol + "@" + emailDomain}
    else
        reg


// 6. Incorporate the new functions into the existing validation system
//(a) After determining that the email has a local part, convert the email address to all lowercase, then
//    canonicalize it.
//(b) Next validate that the (canonical and lowercased) email is not of a blacklisted domain.
//(c) Next validate that the email is unique

let validate3 =
    usernameExists
    >=> emailHasAtSign
    >=> emailHasLocalPart
    >-> lowercaseEmail
    >-> canonicalizeEmail
    >=> emailNotBlacklisted blacklistedDomains
    >=> uniqueEmail existingAccounts

//Create five registration attempt records and pass each to your final validation function chain; print the
//output of each registration attempt. Your five records should involve:

//(a) At least two @gmail.com email addresses that need to be canonicalized in different ways
//(d) At least one email address that has uppercase characters.
let reg1 = {username = "juliantran" ; email = "julian.tran.@gmail.com"} 
let reg2 = {username = "csulbstudent" ; email = "SENIOR+5thyear@gmail.com"}

//(b) At least one email address that is not a gmail address.
//(f) At least one email address with no local part.
//(g) At least one username that is empty.
let reg3 = {username = "" ; email = "@yahoo.com"}

//(c) At least one non-gmail address that uses characters which would be removed during canonicalization if it were a gmail address.
//(h) At least one registration attempt that succeeds.
let reg4 = {username = "bobaworker1" ; email = "its-boba-time-artesia@gmail.com"}

//(e) At least one email address that uses a blacklisted domain.
let reg5 = {username = "techieguy" ; email = "TechAssistant@mailinator.org"}

printfn"Reg1: %A\n" (validate3 reg1)
printfn"Reg2: %A\n" (validate3 reg2)
printfn"Reg3: %A\n" (validate3 reg3)
printfn"Reg4: %A\n" (validate3 reg4)
printfn"Reg5: %A\n" (validate3 reg5)