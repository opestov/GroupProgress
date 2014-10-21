# GroupProgress

The library that allows to get information about progress of a group of
people in solving programming problems on a set of online judges.

This is the typical homework assignment in my class:

> In order to better understand the subject of lecture
> please solve the problem A and B on acm.timus.ru and problem C on informatics.mccme.ru

GroupProgress can be used to track the process of completting such kind of homeworks.

# Entry point

To retrieve information one has to call the method `GroupProgress.Main.gather` and pass it the list of judges and path to the configuration file.

# Config file

````JSON
{
    "name": "Group name",
    "users": [
        { "name": "User 1", "display": "handle1", "timus": 101, "mccme": 12345},
        { "name": "User 2", "display": "handle2", "timus": 102, "oplab": 999},
    ],
    "contests": [
        {
            "name": "Set 1",
            "problems": [
                { "site": "timus", "num": 2000, "desc": "Grand Theft Array V" },
                { "site": "oplab", "contest": 3, "problem": 1,  "link": "http://ejudge.oplab.org/01-firststeps/statements.html", "desc": "Hello, world!" },
                { "site": "mccme", "problem": 2936 }
            ]
        },
        {
            "name": "Set 2",
            "problems": [
                { "site": "timus", "num": 2000 },
            ]
        }
    ]
}
```

# Judges

The library supports set of predifined judges.
Note that interaction with some of them is not based on any API but is a process of sending HTTP GET requests and parsing raw HTML. It means that this interaction will fail when judge owners change format of pages.

##  http://acm.timus.ru

+ Use `GroupProgress.Main.Judge.CreateTimus()` to get provider.
+ To link user with timus author add property `"timus":[Author Id]` to the user description.
+ To add problem to the contest specify the following properties: `"site":"timus", "num":[Problem Id], "desc":[Optional problem description]`.

## http://informatics.mccme.ru

+ Use `GroupProgress.Main.Judge.CreateMccme()` to get provider.
+ To link user with mccme user add property `"mccme":[User Id]` to the user description.
+ To add problem to the contest specify the following properties: `"site":"mccme", "problem":[Problem Id], "desc":[Optional problem description]`.

## https://ejudge.ru

If you have custom installation of ejudge then you can use this provider. It gets data from external.xml files stored on local file system and you have two options.

The first assumes that default ejudge path for external.xml is used: `[dir]/000043/var/status/dir/external.xml`. In this case

+ Use `GroupProgress.Main.Judge.CreateEjudge(string judgeName, string dir)` to get provider.
+ To link user with ejudge user add property `"[judgeName]":[User Id]` to the user description.
+ To add problem to the contest specify the following properties: `"site":"[judgeName]", "contest":[Contest Id], "problem":[Problem Id], "desc":[Optional problem description], "link":[Optional link to the problem statement]`.

Otherwise if all external.xml files stored in one directory (`dir/3.xml, dir/7.xml, dir/43.xml`) then use second option to get provider. That is `GroupProgress.Main.Judge.CreateLocalEjudge(string judgeName, string dir)`.


# Usage

I'm using this library to daily update information about groups listed on the page http://school.oplab.org/groups.html.
The F# script and actual configuration files can be found in [related repository](https://github.com/opestov/school.oplab.org/tree/master/bricks/groups)
