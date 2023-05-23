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
function matcher (regex, string) {
    // building the regex with the linear flag
    const re = new RegExp(regex,"l");
    // matching the regex
    const result = string.match(re);
    console.log(result_to_string(result));
    return 0;
}

const regex = arguments[0];
const string = arguments[1];
matcher(regex, string);
