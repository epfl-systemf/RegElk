// prints a single capture group
function group_to_string (grp) {
    if (grp == null) {
        return "Undefined"
    }
    return grp.toString();
}

// printing the result of a match in the same way we printed it in OCaml
function result_to_string (result) {
    if (result == null) {
        return "NoMatch\n";
    }
    output = "";
    for (var i = 0; i < result.length; i++) {
        output += "#";
        output += i.toString();
        output += ":";
        output += group_to_string(result[i]);
        output += "\n";
    }
    return output;
}

// calling the regex engine
function matcher () {
    // getting the command line arguments
    const regex = process.argv[2];
    const string = process.argv[3];
    // building the regex
    const re = new RegExp(regex);
    // matching the regex
    const result = string.match(re);
    console.log(result_to_string(result));
    return 0;
}

matcher();
