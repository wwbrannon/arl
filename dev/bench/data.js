window.BENCHMARK_DATA = {
  "lastUpdate": 1771839621618,
  "repoUrl": "https://github.com/wwbrannon/arl",
  "entries": {
    "Benchmark": [
      {
        "commit": {
          "author": {
            "username": "wwbrannon"
          },
          "committer": {
            "username": "wwbrannon"
          },
          "id": "d973525",
          "message": "first benchmark baseline",
          "timestamp": "2026-02-06T23:37:09Z",
          "url": "https://github.com/wwbrannon/arl/commit/d973525"
        },
        "date": 1738888554000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "unit": "ms",
            "value": 0.03321003
          },
          {
            "name": "tokenizer/strings/100 chars",
            "unit": "ms",
            "value": 0.2515555
          },
          {
            "name": "tokenizer/strings/1K chars",
            "unit": "ms",
            "value": 2.375786
          },
          {
            "name": "tokenizer/strings/10K chars",
            "unit": "ms",
            "value": 24.8986
          },
          {
            "name": "tokenizer/nested/10 levels",
            "unit": "ms",
            "value": 0.122631
          },
          {
            "name": "tokenizer/nested/50 levels",
            "unit": "ms",
            "value": 0.366212
          },
          {
            "name": "tokenizer/nested/100 levels",
            "unit": "ms",
            "value": 0.675393
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "unit": "ms",
            "value": 0.549564
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "unit": "ms",
            "value": 5.561261
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "unit": "ms",
            "value": 51.73489
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "unit": "ms",
            "value": 0.04108204
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "unit": "ms",
            "value": 0.04395214
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "unit": "ms",
            "value": 1.392873
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "unit": "ms",
            "value": 22.59879
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "unit": "ms",
            "value": 26.95916
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "unit": "ms",
            "value": 34.72926
          },
          {
            "name": "parser/flat/10 elements",
            "unit": "ms",
            "value": 0.136981
          },
          {
            "name": "parser/flat/100 elements",
            "unit": "ms",
            "value": 1.057636
          },
          {
            "name": "parser/flat/1000 elements",
            "unit": "ms",
            "value": 10.72324
          },
          {
            "name": "parser/nested/Depth 5",
            "unit": "ms",
            "value": 0.1559639
          },
          {
            "name": "parser/nested/Depth 10",
            "unit": "ms",
            "value": 0.2971475
          },
          {
            "name": "parser/nested/Depth 20",
            "unit": "ms",
            "value": 0.5923269
          },
          {
            "name": "parser/sugar/Single quote expr",
            "unit": "ms",
            "value": 0.187862
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "unit": "ms",
            "value": 1.896742
          },
          {
            "name": "parser/nulls/Few NULLs",
            "unit": "ms",
            "value": 0.09844103
          },
          {
            "name": "parser/nulls/100 NULLs",
            "unit": "ms",
            "value": 1.159644
          },
          {
            "name": "parser/real/fibonacci.arl",
            "unit": "ms",
            "value": 6.966843
          },
          {
            "name": "parser/real/quicksort.arl",
            "unit": "ms",
            "value": 8.305944
          },
          {
            "name": "parser/real/macro-examples.arl",
            "unit": "ms",
            "value": 10.07099
          },
          {
            "name": "macro/simple/Simple macro",
            "unit": "ms",
            "value": 0.404465
          },
          {
            "name": "macro/complex/Complex macro",
            "unit": "ms",
            "value": 0.830824
          },
          {
            "name": "macro/nested/Nested macros",
            "unit": "ms",
            "value": 1.584671
          },
          {
            "name": "macro/hygiene/With hygiene",
            "unit": "ms",
            "value": 1.764046
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "unit": "ms",
            "value": 11.65944
          },
          {
            "name": "macro/real/macro-examples.arl",
            "unit": "ms",
            "value": 506.294
          },
          {
            "name": "eval/arithmetic/Single add",
            "unit": "ms",
            "value": 0.3671141
          },
          {
            "name": "eval/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.785683
          },
          {
            "name": "eval/arithmetic/Many adds",
            "unit": "ms",
            "value": 2.073288
          },
          {
            "name": "eval/calls/1 arg",
            "unit": "ms",
            "value": 0.2924736
          },
          {
            "name": "eval/calls/5 args",
            "unit": "ms",
            "value": 0.6286325
          },
          {
            "name": "eval/calls/10 args",
            "unit": "ms",
            "value": 1.031478
          },
          {
            "name": "eval/special/if",
            "unit": "ms",
            "value": 0.4316275
          },
          {
            "name": "eval/special/define",
            "unit": "ms",
            "value": 0.5271779
          },
          {
            "name": "eval/special/lambda",
            "unit": "ms",
            "value": 0.6201661
          },
          {
            "name": "eval/special/begin",
            "unit": "ms",
            "value": 0.4367731
          },
          {
            "name": "eval/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 4.18159
          },
          {
            "name": "eval/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 9.667451
          },
          {
            "name": "eval/recursive/factorial(100)",
            "unit": "ms",
            "value": 2.673774
          },
          {
            "name": "eval/recursive/factorial(500)",
            "unit": "ms",
            "value": 12.82296
          },
          {
            "name": "eval/closures/Create closure",
            "unit": "ms",
            "value": 0.3451791
          },
          {
            "name": "eval/closures/Call closure",
            "unit": "ms",
            "value": 0.476625
          },
          {
            "name": "eval/real/fibonacci.arl",
            "unit": "ms",
            "value": 419.5007
          },
          {
            "name": "eval/real/quicksort.arl",
            "unit": "ms",
            "value": 496.0655
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "unit": "ms",
            "value": 0.2890499
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "unit": "ms",
            "value": 0.5854389
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "unit": "ms",
            "value": 0.2807269
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "unit": "ms",
            "value": 0.2836791
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "unit": "ms",
            "value": 0.2869589
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "unit": "ms",
            "value": 0.3146339
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "unit": "ms",
            "value": 0.4593231
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "unit": "ms",
            "value": 0.491385
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "unit": "ms",
            "value": 1.408043
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "unit": "ms",
            "value": 10.22312
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "unit": "ms",
            "value": 0.4531321
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "unit": "ms",
            "value": 1.089063
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "unit": "ms",
            "value": 0.552188
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "unit": "ms",
            "value": 1.265486
          },
          {
            "name": "stdlib/compose/Single function",
            "unit": "ms",
            "value": 0.2921046
          },
          {
            "name": "stdlib/compose/Two composed",
            "unit": "ms",
            "value": 0.4239605
          },
          {
            "name": "stdlib/compose/Three composed",
            "unit": "ms",
            "value": 0.552147
          },
          {
            "name": "stdlib/strings/str (2 args)",
            "unit": "ms",
            "value": 3.796621
          },
          {
            "name": "stdlib/strings/str (5 args)",
            "unit": "ms",
            "value": 8.862724
          },
          {
            "name": "stdlib/strings/str (10 args)",
            "unit": "ms",
            "value": 19.10426
          },
          {
            "name": "stdlib/predicates/null?",
            "unit": "ms",
            "value": 0.2789026
          },
          {
            "name": "stdlib/predicates/list?",
            "unit": "ms",
            "value": 0.5615565
          },
          {
            "name": "stdlib/predicates/number?",
            "unit": "ms",
            "value": 0.2879635
          },
          {
            "name": "stdlib/predicates/string?",
            "unit": "ms",
            "value": 0.249116
          },
          {
            "name": "stdlib/construction/cons (single)",
            "unit": "ms",
            "value": 0.581093
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "unit": "ms",
            "value": 1.9311
          },
          {
            "name": "stdlib/construction/append (large)",
            "unit": "ms",
            "value": 2.850484
          },
          {
            "name": "stdlib/construction/range (10)",
            "unit": "ms",
            "value": 0.5781821
          },
          {
            "name": "stdlib/construction/range (100)",
            "unit": "ms",
            "value": 0.725946
          },
          {
            "name": "stdlib/nested/map then filter",
            "unit": "ms",
            "value": 0.727996
          },
          {
            "name": "stdlib/nested/filter then map",
            "unit": "ms",
            "value": 0.6832241
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "unit": "ms",
            "value": 1.064483
          },
          {
            "name": "interop/calls/mean positional",
            "unit": "ms",
            "value": 1.103925
          },
          {
            "name": "interop/calls/mean named",
            "unit": "ms",
            "value": 1.144412
          },
          {
            "name": "interop/calls/seq positional",
            "unit": "ms",
            "value": 0.477076
          },
          {
            "name": "interop/calls/seq named",
            "unit": "ms",
            "value": 0.5616795
          },
          {
            "name": "interop/objects/vector",
            "unit": "ms",
            "value": 0.9950289
          },
          {
            "name": "interop/objects/list",
            "unit": "ms",
            "value": 1.040375
          },
          {
            "name": "interop/objects/data.frame",
            "unit": "ms",
            "value": 1.140579
          },
          {
            "name": "interop/objects/formula",
            "unit": "ms",
            "value": 0.3717265
          },
          {
            "name": "modules/import/import binding",
            "unit": "ms",
            "value": 1314.776
          },
          {
            "name": "modules/import/import control",
            "unit": "ms",
            "value": 1310.824
          },
          {
            "name": "modules/load_run/load",
            "unit": "ms",
            "value": 1298.868
          },
          {
            "name": "modules/load_run/run",
            "unit": "ms",
            "value": 1324.567
          },
          {
            "name": "e2e/synthetic/Micro",
            "unit": "ms",
            "value": 1338.518
          },
          {
            "name": "e2e/synthetic/Small",
            "unit": "ms",
            "value": 1319.635
          },
          {
            "name": "e2e/synthetic/Medium",
            "unit": "ms",
            "value": 1389.525
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "unit": "ms",
            "value": 1338.353
          },
          {
            "name": "e2e/strings/1K string",
            "unit": "ms",
            "value": 1277.089
          },
          {
            "name": "e2e/strings/10K string",
            "unit": "ms",
            "value": 1333.116
          },
          {
            "name": "e2e/args/50 args",
            "unit": "ms",
            "value": 1280.531
          },
          {
            "name": "e2e/args/100 args",
            "unit": "ms",
            "value": 1289.522
          },
          {
            "name": "e2e/repl/REPL session",
            "unit": "ms",
            "value": 1271.755
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "unit": "ms",
            "value": 1634.033
          },
          {
            "name": "e2e/real/quicksort.arl",
            "unit": "ms",
            "value": 1714.057
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "unit": "ms",
            "value": 1726.341
          }
        ]
      },
      {
        "commit": {
          "author": {
            "username": "wwbrannon"
          },
          "committer": {
            "username": "wwbrannon"
          },
          "id": "effbe9f",
          "message": "second benchmark baseline",
          "timestamp": "2026-02-10T07:53:35Z",
          "url": "https://github.com/wwbrannon/arl/commit/effbe9f"
        },
        "date": 1739177613000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "unit": "ms",
            "value": 0.03521913
          },
          {
            "name": "tokenizer/strings/100 chars",
            "unit": "ms",
            "value": 0.05030702
          },
          {
            "name": "tokenizer/strings/1K chars",
            "unit": "ms",
            "value": 0.157563
          },
          {
            "name": "tokenizer/strings/10K chars",
            "unit": "ms",
            "value": 1.242177
          },
          {
            "name": "tokenizer/nested/10 levels",
            "unit": "ms",
            "value": 0.1043861
          },
          {
            "name": "tokenizer/nested/50 levels",
            "unit": "ms",
            "value": 0.2296818
          },
          {
            "name": "tokenizer/nested/100 levels",
            "unit": "ms",
            "value": 0.386466
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "unit": "ms",
            "value": 0.4475971
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "unit": "ms",
            "value": 4.267977
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "unit": "ms",
            "value": 42.70047
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "unit": "ms",
            "value": 0.03534206
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "unit": "ms",
            "value": 0.03591622
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "unit": "ms",
            "value": 0.09680097
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "unit": "ms",
            "value": 14.68608
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "unit": "ms",
            "value": 18.43549
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "unit": "ms",
            "value": 18.91785
          },
          {
            "name": "parser/flat/10 elements",
            "unit": "ms",
            "value": 0.1327579
          },
          {
            "name": "parser/flat/100 elements",
            "unit": "ms",
            "value": 1.041216
          },
          {
            "name": "parser/flat/1000 elements",
            "unit": "ms",
            "value": 10.39742
          },
          {
            "name": "parser/nested/Depth 5",
            "unit": "ms",
            "value": 0.151413
          },
          {
            "name": "parser/nested/Depth 10",
            "unit": "ms",
            "value": 0.2892551
          },
          {
            "name": "parser/nested/Depth 20",
            "unit": "ms",
            "value": 0.5716016
          },
          {
            "name": "parser/sugar/Single quote expr",
            "unit": "ms",
            "value": 0.1758081
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "unit": "ms",
            "value": 1.734915
          },
          {
            "name": "parser/nulls/Few NULLs",
            "unit": "ms",
            "value": 0.09319303
          },
          {
            "name": "parser/nulls/100 NULLs",
            "unit": "ms",
            "value": 1.071248
          },
          {
            "name": "parser/real/fibonacci.arl",
            "unit": "ms",
            "value": 6.443724
          },
          {
            "name": "parser/real/quicksort.arl",
            "unit": "ms",
            "value": 7.864723
          },
          {
            "name": "parser/real/macro-examples.arl",
            "unit": "ms",
            "value": 9.225041
          },
          {
            "name": "macro/simple/Simple macro",
            "unit": "ms",
            "value": 0.4510002
          },
          {
            "name": "macro/complex/Complex macro",
            "unit": "ms",
            "value": 0.7974706
          },
          {
            "name": "macro/nested/Nested macros",
            "unit": "ms",
            "value": 1.638278
          },
          {
            "name": "macro/hygiene/With hygiene",
            "unit": "ms",
            "value": 1.17752
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "unit": "ms",
            "value": 6.454302
          },
          {
            "name": "macro/real/macro-examples.arl",
            "unit": "ms",
            "value": 163.9165
          },
          {
            "name": "eval/arithmetic/Single add",
            "unit": "ms",
            "value": 0.3590985
          },
          {
            "name": "eval/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.7460361
          },
          {
            "name": "eval/arithmetic/Many adds",
            "unit": "ms",
            "value": 1.906459
          },
          {
            "name": "eval/calls/1 arg",
            "unit": "ms",
            "value": 0.28577
          },
          {
            "name": "eval/calls/5 args",
            "unit": "ms",
            "value": 0.5974111
          },
          {
            "name": "eval/calls/10 args",
            "unit": "ms",
            "value": 0.9791621
          },
          {
            "name": "eval/special/if",
            "unit": "ms",
            "value": 0.3490535
          },
          {
            "name": "eval/special/define",
            "unit": "ms",
            "value": 0.3486231
          },
          {
            "name": "eval/special/lambda",
            "unit": "ms",
            "value": 0.5734465
          },
          {
            "name": "eval/special/begin",
            "unit": "ms",
            "value": 0.3986021
          },
          {
            "name": "eval/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 3.506566
          },
          {
            "name": "eval/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 8.760634
          },
          {
            "name": "eval/recursive/factorial(100)",
            "unit": "ms",
            "value": 2.737324
          },
          {
            "name": "eval/recursive/factorial(500)",
            "unit": "ms",
            "value": 12.92367
          },
          {
            "name": "eval/closures/Create closure",
            "unit": "ms",
            "value": 0.216234
          },
          {
            "name": "eval/closures/Call closure",
            "unit": "ms",
            "value": 0.235914
          },
          {
            "name": "eval/real/fibonacci.arl",
            "unit": "ms",
            "value": 122.5692
          },
          {
            "name": "eval/real/quicksort.arl",
            "unit": "ms",
            "value": 155.4289
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "unit": "ms",
            "value": 0.2788615
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "unit": "ms",
            "value": 0.33661
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "unit": "ms",
            "value": 0.2752126
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "unit": "ms",
            "value": 0.2752536
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "unit": "ms",
            "value": 0.2789641
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "unit": "ms",
            "value": 0.3010221
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "unit": "ms",
            "value": 0.4450346
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "unit": "ms",
            "value": 0.4564531
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "unit": "ms",
            "value": 1.324608
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "unit": "ms",
            "value": 10.60311
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "unit": "ms",
            "value": 0.4442965
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "unit": "ms",
            "value": 1.165651
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "unit": "ms",
            "value": 0.521807
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "unit": "ms",
            "value": 1.263907
          },
          {
            "name": "stdlib/compose/Single function",
            "unit": "ms",
            "value": 0.2890909
          },
          {
            "name": "stdlib/compose/Two composed",
            "unit": "ms",
            "value": 0.4088725
          },
          {
            "name": "stdlib/compose/Three composed",
            "unit": "ms",
            "value": 0.5296995
          },
          {
            "name": "stdlib/strings/str (2 args)",
            "unit": "ms",
            "value": 0.679862
          },
          {
            "name": "stdlib/strings/str (5 args)",
            "unit": "ms",
            "value": 1.320856
          },
          {
            "name": "stdlib/strings/str (10 args)",
            "unit": "ms",
            "value": 2.370887
          },
          {
            "name": "stdlib/predicates/null?",
            "unit": "ms",
            "value": 0.219678
          },
          {
            "name": "stdlib/predicates/list?",
            "unit": "ms",
            "value": 0.5328769
          },
          {
            "name": "stdlib/predicates/number?",
            "unit": "ms",
            "value": 0.2698209
          },
          {
            "name": "stdlib/predicates/string?",
            "unit": "ms",
            "value": 0.2368982
          },
          {
            "name": "stdlib/construction/cons (single)",
            "unit": "ms",
            "value": 0.5385759
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "unit": "ms",
            "value": 0.868954
          },
          {
            "name": "stdlib/construction/append (large)",
            "unit": "ms",
            "value": 1.149804
          },
          {
            "name": "stdlib/construction/range (10)",
            "unit": "ms",
            "value": 0.4245141
          },
          {
            "name": "stdlib/construction/range (100)",
            "unit": "ms",
            "value": 0.5730775
          },
          {
            "name": "stdlib/nested/map then filter",
            "unit": "ms",
            "value": 0.700444
          },
          {
            "name": "stdlib/nested/filter then map",
            "unit": "ms",
            "value": 0.664774
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "unit": "ms",
            "value": 1.012516
          },
          {
            "name": "interop/calls/mean positional",
            "unit": "ms",
            "value": 1.05165
          },
          {
            "name": "interop/calls/mean named",
            "unit": "ms",
            "value": 1.078054
          },
          {
            "name": "interop/calls/seq positional",
            "unit": "ms",
            "value": 0.464407
          },
          {
            "name": "interop/calls/seq named",
            "unit": "ms",
            "value": 0.5241851
          },
          {
            "name": "interop/objects/vector",
            "unit": "ms",
            "value": 0.936399
          },
          {
            "name": "interop/objects/list",
            "unit": "ms",
            "value": 0.9574934
          },
          {
            "name": "interop/objects/data.frame",
            "unit": "ms",
            "value": 1.038386
          },
          {
            "name": "interop/objects/formula",
            "unit": "ms",
            "value": 0.3497916
          },
          {
            "name": "modules/import/import binding",
            "unit": "ms",
            "value": 82.29299
          },
          {
            "name": "modules/import/import control",
            "unit": "ms",
            "value": 77.85701
          },
          {
            "name": "modules/load_run/load",
            "unit": "ms",
            "value": 80.61578
          },
          {
            "name": "modules/load_run/run",
            "unit": "ms",
            "value": 79.46366
          },
          {
            "name": "e2e/synthetic/Micro",
            "unit": "ms",
            "value": 83.09599
          },
          {
            "name": "e2e/synthetic/Small",
            "unit": "ms",
            "value": 83.29316
          },
          {
            "name": "e2e/synthetic/Medium",
            "unit": "ms",
            "value": 117.1897
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "unit": "ms",
            "value": 96.60463
          },
          {
            "name": "e2e/strings/1K string",
            "unit": "ms",
            "value": 76.87176
          },
          {
            "name": "e2e/strings/10K string",
            "unit": "ms",
            "value": 79.61944
          },
          {
            "name": "e2e/args/50 args",
            "unit": "ms",
            "value": 79.37612
          },
          {
            "name": "e2e/args/100 args",
            "unit": "ms",
            "value": 88.10787
          },
          {
            "name": "e2e/repl/REPL session",
            "unit": "ms",
            "value": 79.95223
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "unit": "ms",
            "value": 204.1272
          },
          {
            "name": "e2e/real/quicksort.arl",
            "unit": "ms",
            "value": 254.4768
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "unit": "ms",
            "value": 228.1942
          }
        ]
      },
      {
        "commit": {
          "author": {
            "username": "William Brannon"
          },
          "committer": {
            "username": "William Brannon"
          },
          "id": "0f81844",
          "message": "fix up benchmark vignette",
          "timestamp": "2026-02-14T05:45:57-05:00",
          "url": "https://github.com/wwbrannon/arl/commit/0f81844"
        },
        "date": 1771061898000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "unit": "ms",
            "value": 0.0344795
          },
          {
            "name": "tokenizer/strings/100 chars",
            "unit": "ms",
            "value": 0.044667
          },
          {
            "name": "tokenizer/strings/1K chars",
            "unit": "ms",
            "value": 0.137084
          },
          {
            "name": "tokenizer/strings/10K chars",
            "unit": "ms",
            "value": 1.076604
          },
          {
            "name": "tokenizer/nested/10 levels",
            "unit": "ms",
            "value": 0.097084
          },
          {
            "name": "tokenizer/nested/50 levels",
            "unit": "ms",
            "value": 0.232751
          },
          {
            "name": "tokenizer/nested/100 levels",
            "unit": "ms",
            "value": 0.3940835
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "unit": "ms",
            "value": 0.389751
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "unit": "ms",
            "value": 3.743542
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "unit": "ms",
            "value": 38.4215
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "unit": "ms",
            "value": 0.034167
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "unit": "ms",
            "value": 0.035542
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "unit": "ms",
            "value": 0.087001
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "unit": "ms",
            "value": 14.94304
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "unit": "ms",
            "value": 22.82133
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "unit": "ms",
            "value": 15.9354
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "unit": "ms",
            "value": 17.46025
          },
          {
            "name": "parser/flat/10 elements",
            "unit": "ms",
            "value": 0.143792
          },
          {
            "name": "parser/flat/100 elements",
            "unit": "ms",
            "value": 1.168
          },
          {
            "name": "parser/flat/1000 elements",
            "unit": "ms",
            "value": 11.42721
          },
          {
            "name": "parser/nested/Depth 5",
            "unit": "ms",
            "value": 0.160292
          },
          {
            "name": "parser/nested/Depth 10",
            "unit": "ms",
            "value": 0.3117295
          },
          {
            "name": "parser/nested/Depth 20",
            "unit": "ms",
            "value": 0.6153125
          },
          {
            "name": "parser/sugar/Single quote expr",
            "unit": "ms",
            "value": 0.192917
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "unit": "ms",
            "value": 1.948751
          },
          {
            "name": "parser/nulls/Few NULLs",
            "unit": "ms",
            "value": 0.102542
          },
          {
            "name": "parser/nulls/100 NULLs",
            "unit": "ms",
            "value": 1.199501
          },
          {
            "name": "parser/real/fibonacci.arl",
            "unit": "ms",
            "value": 7.631583
          },
          {
            "name": "parser/real/quicksort.arl",
            "unit": "ms",
            "value": 11.34546
          },
          {
            "name": "parser/real/graph-paths.arl",
            "unit": "ms",
            "value": 9.035688
          },
          {
            "name": "parser/real/macro-examples.arl",
            "unit": "ms",
            "value": 10.33417
          },
          {
            "name": "macro/simple/Simple macro",
            "unit": "ms",
            "value": 0.5292085
          },
          {
            "name": "macro/complex/Complex macro",
            "unit": "ms",
            "value": 0.902959
          },
          {
            "name": "macro/nested/Nested macros",
            "unit": "ms",
            "value": 1.875146
          },
          {
            "name": "macro/hygiene/With hygiene",
            "unit": "ms",
            "value": 1.341938
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "unit": "ms",
            "value": 6.715333
          },
          {
            "name": "macro/real/fibonacci.arl",
            "unit": "ms",
            "value": 82.81719
          },
          {
            "name": "macro/real/quicksort.arl",
            "unit": "ms",
            "value": 114.1342
          },
          {
            "name": "macro/real/graph-paths.arl",
            "unit": "ms",
            "value": 44.12277
          },
          {
            "name": "macro/real/macro-examples.arl",
            "unit": "ms",
            "value": 36.54267
          },
          {
            "name": "eval/arithmetic/Single add",
            "unit": "ms",
            "value": 0.3894795
          },
          {
            "name": "eval/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.774938
          },
          {
            "name": "eval/arithmetic/Many adds",
            "unit": "ms",
            "value": 1.952021
          },
          {
            "name": "eval/calls/1 arg",
            "unit": "ms",
            "value": 0.315917
          },
          {
            "name": "eval/calls/5 args",
            "unit": "ms",
            "value": 0.643063
          },
          {
            "name": "eval/calls/10 args",
            "unit": "ms",
            "value": 1.018334
          },
          {
            "name": "eval/special/if",
            "unit": "ms",
            "value": 0.391333
          },
          {
            "name": "eval/special/define",
            "unit": "ms",
            "value": 0.397584
          },
          {
            "name": "eval/special/lambda",
            "unit": "ms",
            "value": 0.6397505
          },
          {
            "name": "eval/special/begin",
            "unit": "ms",
            "value": 0.4685005
          },
          {
            "name": "eval/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 4.000459
          },
          {
            "name": "eval/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 9.604396
          },
          {
            "name": "eval/recursive/factorial(100)",
            "unit": "ms",
            "value": 3.09575
          },
          {
            "name": "eval/recursive/factorial(500)",
            "unit": "ms",
            "value": 13.51606
          },
          {
            "name": "eval/closures/Create closure",
            "unit": "ms",
            "value": 0.274751
          },
          {
            "name": "eval/closures/Call closure",
            "unit": "ms",
            "value": 0.2932715
          },
          {
            "name": "eval/real/fibonacci.arl",
            "unit": "ms",
            "value": 150.8911
          },
          {
            "name": "eval/real/quicksort.arl",
            "unit": "ms",
            "value": 186.0062
          },
          {
            "name": "eval/real/graph-paths.arl",
            "unit": "ms",
            "value": 70.9599
          },
          {
            "name": "eval/real/macro-examples.arl",
            "unit": "ms",
            "value": 211.9286
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "unit": "ms",
            "value": 0.3306255
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "unit": "ms",
            "value": 0.3977295
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "unit": "ms",
            "value": 0.3197295
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "unit": "ms",
            "value": 0.322896
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "unit": "ms",
            "value": 0.3065215
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "unit": "ms",
            "value": 0.340709
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "unit": "ms",
            "value": 0.507646
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "unit": "ms",
            "value": 0.5097715
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "unit": "ms",
            "value": 1.496125
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "unit": "ms",
            "value": 11.26648
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "unit": "ms",
            "value": 0.5068545
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "unit": "ms",
            "value": 1.319313
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "unit": "ms",
            "value": 0.5887505
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "unit": "ms",
            "value": 1.436396
          },
          {
            "name": "stdlib/compose/Single function",
            "unit": "ms",
            "value": 0.327542
          },
          {
            "name": "stdlib/compose/Two composed",
            "unit": "ms",
            "value": 0.456896
          },
          {
            "name": "stdlib/compose/Three composed",
            "unit": "ms",
            "value": 0.5914165
          },
          {
            "name": "stdlib/strings/str (2 args)",
            "unit": "ms",
            "value": 0.824542
          },
          {
            "name": "stdlib/strings/str (5 args)",
            "unit": "ms",
            "value": 1.615959
          },
          {
            "name": "stdlib/strings/str (10 args)",
            "unit": "ms",
            "value": 2.751355
          },
          {
            "name": "stdlib/predicates/null?",
            "unit": "ms",
            "value": 0.258271
          },
          {
            "name": "stdlib/predicates/list?",
            "unit": "ms",
            "value": 0.570938
          },
          {
            "name": "stdlib/predicates/number?",
            "unit": "ms",
            "value": 0.308459
          },
          {
            "name": "stdlib/predicates/string?",
            "unit": "ms",
            "value": 0.279583
          },
          {
            "name": "stdlib/construction/cons (single)",
            "unit": "ms",
            "value": 0.588042
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "unit": "ms",
            "value": 0.959375
          },
          {
            "name": "stdlib/construction/append (large)",
            "unit": "ms",
            "value": 1.280334
          },
          {
            "name": "stdlib/construction/range (10)",
            "unit": "ms",
            "value": 0.496646
          },
          {
            "name": "stdlib/construction/range (100)",
            "unit": "ms",
            "value": 0.6473755
          },
          {
            "name": "stdlib/nested/map then filter",
            "unit": "ms",
            "value": 0.776854
          },
          {
            "name": "stdlib/nested/filter then map",
            "unit": "ms",
            "value": 0.7312085
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "unit": "ms",
            "value": 1.103125
          },
          {
            "name": "interop/calls/mean positional",
            "unit": "ms",
            "value": 1.091771
          },
          {
            "name": "interop/calls/mean named",
            "unit": "ms",
            "value": 1.125709
          },
          {
            "name": "interop/calls/seq positional",
            "unit": "ms",
            "value": 0.5007295
          },
          {
            "name": "interop/calls/seq named",
            "unit": "ms",
            "value": 0.573896
          },
          {
            "name": "interop/objects/vector",
            "unit": "ms",
            "value": 0.9588125
          },
          {
            "name": "interop/objects/list",
            "unit": "ms",
            "value": 0.9801875
          },
          {
            "name": "interop/objects/data.frame",
            "unit": "ms",
            "value": 1.106604
          },
          {
            "name": "interop/objects/formula",
            "unit": "ms",
            "value": 0.3750215
          },
          {
            "name": "modules/import/import binding",
            "unit": "ms",
            "value": 125.4059
          },
          {
            "name": "modules/import/import control",
            "unit": "ms",
            "value": 119.7663
          },
          {
            "name": "modules/load_run/load",
            "unit": "ms",
            "value": 123.1911
          },
          {
            "name": "modules/load_run/run",
            "unit": "ms",
            "value": 125.6083
          },
          {
            "name": "e2e/synthetic/Micro",
            "unit": "ms",
            "value": 119.3232
          },
          {
            "name": "e2e/synthetic/Small",
            "unit": "ms",
            "value": 129.8024
          },
          {
            "name": "e2e/synthetic/Medium",
            "unit": "ms",
            "value": 203.1986
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "unit": "ms",
            "value": 139.6068
          },
          {
            "name": "e2e/strings/1K string",
            "unit": "ms",
            "value": 121.677
          },
          {
            "name": "e2e/strings/10K string",
            "unit": "ms",
            "value": 118.2881
          },
          {
            "name": "e2e/args/50 args",
            "unit": "ms",
            "value": 124.3229
          },
          {
            "name": "e2e/args/100 args",
            "unit": "ms",
            "value": 127.1958
          },
          {
            "name": "e2e/repl/REPL session",
            "unit": "ms",
            "value": 120.3176
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "unit": "ms",
            "value": 279.6071
          },
          {
            "name": "e2e/real/quicksort.arl",
            "unit": "ms",
            "value": 349.5292
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "unit": "ms",
            "value": 233.4186
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "unit": "ms",
            "value": 356.1974
          }
        ]
      },
      {
        "commit": {
          "author": {
            "username": "William Brannon"
          },
          "committer": {
            "username": "William Brannon"
          },
          "id": "e3c7200",
          "message": "Extract inline R code from publish-results.sh to separate script",
          "timestamp": "2026-02-15T00:24:40Z",
          "url": "https://github.com/wwbrannon/arl/commit/e3c7200"
        },
        "date": 1771126477000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "unit": "ms",
            "value": 0.03603916
          },
          {
            "name": "tokenizer/strings/100 chars",
            "unit": "ms",
            "value": 0.05075801
          },
          {
            "name": "tokenizer/strings/1K chars",
            "unit": "ms",
            "value": 0.156784
          },
          {
            "name": "tokenizer/strings/10K chars",
            "unit": "ms",
            "value": 1.228483
          },
          {
            "name": "tokenizer/nested/10 levels",
            "unit": "ms",
            "value": 0.1046322
          },
          {
            "name": "tokenizer/nested/50 levels",
            "unit": "ms",
            "value": 0.2307892
          },
          {
            "name": "tokenizer/nested/100 levels",
            "unit": "ms",
            "value": 0.3841701
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "unit": "ms",
            "value": 0.44526
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "unit": "ms",
            "value": 4.238047
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "unit": "ms",
            "value": 43.26763
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "unit": "ms",
            "value": 0.03890903
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "unit": "ms",
            "value": 0.03919587
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "unit": "ms",
            "value": 0.09926106
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "unit": "ms",
            "value": 17.2436
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "unit": "ms",
            "value": 26.58307
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "unit": "ms",
            "value": 18.63356
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "unit": "ms",
            "value": 19.69533
          },
          {
            "name": "parser/flat/10 elements",
            "unit": "ms",
            "value": 0.1318969
          },
          {
            "name": "parser/flat/100 elements",
            "unit": "ms",
            "value": 1.039698
          },
          {
            "name": "parser/flat/1000 elements",
            "unit": "ms",
            "value": 10.50022
          },
          {
            "name": "parser/nested/Depth 5",
            "unit": "ms",
            "value": 0.1480512
          },
          {
            "name": "parser/nested/Depth 10",
            "unit": "ms",
            "value": 0.2822235
          },
          {
            "name": "parser/nested/Depth 20",
            "unit": "ms",
            "value": 0.557231
          },
          {
            "name": "parser/sugar/Single quote expr",
            "unit": "ms",
            "value": 0.172569
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "unit": "ms",
            "value": 1.716773
          },
          {
            "name": "parser/nulls/Few NULLs",
            "unit": "ms",
            "value": 0.09368511
          },
          {
            "name": "parser/nulls/100 NULLs",
            "unit": "ms",
            "value": 1.078075
          },
          {
            "name": "parser/real/fibonacci.arl",
            "unit": "ms",
            "value": 6.784598
          },
          {
            "name": "parser/real/quicksort.arl",
            "unit": "ms",
            "value": 9.89125
          },
          {
            "name": "parser/real/graph-paths.arl",
            "unit": "ms",
            "value": 7.971876
          },
          {
            "name": "parser/real/macro-examples.arl",
            "unit": "ms",
            "value": 9.201835
          },
          {
            "name": "macro/simple/Simple macro",
            "unit": "ms",
            "value": 0.4848249
          },
          {
            "name": "macro/complex/Complex macro",
            "unit": "ms",
            "value": 0.8279951
          },
          {
            "name": "macro/nested/Nested macros",
            "unit": "ms",
            "value": 1.686063
          },
          {
            "name": "macro/hygiene/With hygiene",
            "unit": "ms",
            "value": 1.161469
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "unit": "ms",
            "value": 5.862016
          },
          {
            "name": "macro/real/fibonacci.arl",
            "unit": "ms",
            "value": 75.5424
          },
          {
            "name": "macro/real/quicksort.arl",
            "unit": "ms",
            "value": 107.3313
          },
          {
            "name": "macro/real/graph-paths.arl",
            "unit": "ms",
            "value": 41.82449
          },
          {
            "name": "macro/real/macro-examples.arl",
            "unit": "ms",
            "value": 35.61912
          },
          {
            "name": "compile/arithmetic/Single add",
            "unit": "ms",
            "value": 0.056908
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.138498
          },
          {
            "name": "compile/arithmetic/Many adds",
            "unit": "ms",
            "value": 0.386712
          },
          {
            "name": "compile/calls/1 arg",
            "unit": "ms",
            "value": 0.04350091
          },
          {
            "name": "compile/calls/5 args",
            "unit": "ms",
            "value": 0.07884321
          },
          {
            "name": "compile/calls/10 args",
            "unit": "ms",
            "value": 0.1225695
          },
          {
            "name": "compile/special/if",
            "unit": "ms",
            "value": 0.04231208
          },
          {
            "name": "compile/special/define",
            "unit": "ms",
            "value": 0.050348
          },
          {
            "name": "compile/special/lambda",
            "unit": "ms",
            "value": 0.08583348
          },
          {
            "name": "compile/special/begin",
            "unit": "ms",
            "value": 0.05083997
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 0.04575611
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 0.04391116
          },
          {
            "name": "compile/recursive/factorial(100)",
            "unit": "ms",
            "value": 0.04370604
          },
          {
            "name": "compile/recursive/factorial(500)",
            "unit": "ms",
            "value": 0.04368555
          },
          {
            "name": "compile/closures/Create closure",
            "unit": "ms",
            "value": 0.03505498
          },
          {
            "name": "compile/real/fibonacci.arl",
            "unit": "ms",
            "value": 45.5
          },
          {
            "name": "compile/real/quicksort.arl",
            "unit": "ms",
            "value": 43
          },
          {
            "name": "compile/real/graph-paths.arl",
            "unit": "ms",
            "value": 46
          },
          {
            "name": "compile/real/macro-examples.arl",
            "unit": "ms",
            "value": 8
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "unit": "ms",
            "value": 0.01447299
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.01443201
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "unit": "ms",
            "value": 0.01514948
          },
          {
            "name": "r-eval/calls/1 arg",
            "unit": "ms",
            "value": 0.01484202
          },
          {
            "name": "r-eval/calls/5 args",
            "unit": "ms",
            "value": 0.0470269
          },
          {
            "name": "r-eval/calls/10 args",
            "unit": "ms",
            "value": 0.08626399
          },
          {
            "name": "r-eval/special/if",
            "unit": "ms",
            "value": 0.01461653
          },
          {
            "name": "r-eval/special/define",
            "unit": "ms",
            "value": 0.02025417
          },
          {
            "name": "r-eval/special/lambda",
            "unit": "ms",
            "value": 0.01656392
          },
          {
            "name": "r-eval/special/begin",
            "unit": "ms",
            "value": 0.01566205
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 3.365198
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 9.167415
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "unit": "ms",
            "value": 2.53052
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "unit": "ms",
            "value": 14.2936
          },
          {
            "name": "r-eval/closures/Create closure",
            "unit": "ms",
            "value": 0.02431287
          },
          {
            "name": "r-eval/closures/Call closure",
            "unit": "ms",
            "value": 0.04417752
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "unit": "ms",
            "value": 63.5
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "unit": "ms",
            "value": 65
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "unit": "ms",
            "value": 21.5
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "unit": "ms",
            "value": 43
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "unit": "ms",
            "value": 0.3226085
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "unit": "ms",
            "value": 0.3806235
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "unit": "ms",
            "value": 0.3089554
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "unit": "ms",
            "value": 0.3041381
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "unit": "ms",
            "value": 0.3082381
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "unit": "ms",
            "value": 0.3356669
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "unit": "ms",
            "value": 0.5012661
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "unit": "ms",
            "value": 0.548252
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "unit": "ms",
            "value": 1.465156
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "unit": "ms",
            "value": 13.24739
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "unit": "ms",
            "value": 0.476625
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "unit": "ms",
            "value": 1.407817
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "unit": "ms",
            "value": 0.5711709
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "unit": "ms",
            "value": 1.325796
          },
          {
            "name": "stdlib/compose/Single function",
            "unit": "ms",
            "value": 0.3126867
          },
          {
            "name": "stdlib/compose/Two composed",
            "unit": "ms",
            "value": 0.427179
          },
          {
            "name": "stdlib/compose/Three composed",
            "unit": "ms",
            "value": 0.5476575
          },
          {
            "name": "stdlib/strings/str (2 args)",
            "unit": "ms",
            "value": 0.702822
          },
          {
            "name": "stdlib/strings/str (5 args)",
            "unit": "ms",
            "value": 1.344144
          },
          {
            "name": "stdlib/strings/str (10 args)",
            "unit": "ms",
            "value": 2.413486
          },
          {
            "name": "stdlib/predicates/null?",
            "unit": "ms",
            "value": 0.242351
          },
          {
            "name": "stdlib/predicates/list?",
            "unit": "ms",
            "value": 0.552393
          },
          {
            "name": "stdlib/predicates/number?",
            "unit": "ms",
            "value": 0.2921661
          },
          {
            "name": "stdlib/predicates/string?",
            "unit": "ms",
            "value": 0.2589561
          },
          {
            "name": "stdlib/construction/cons (single)",
            "unit": "ms",
            "value": 0.5638935
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "unit": "ms",
            "value": 0.884206
          },
          {
            "name": "stdlib/construction/append (large)",
            "unit": "ms",
            "value": 1.161243
          },
          {
            "name": "stdlib/construction/range (10)",
            "unit": "ms",
            "value": 0.4483557
          },
          {
            "name": "stdlib/construction/range (100)",
            "unit": "ms",
            "value": 0.5908511
          },
          {
            "name": "stdlib/nested/map then filter",
            "unit": "ms",
            "value": 0.7155321
          },
          {
            "name": "stdlib/nested/filter then map",
            "unit": "ms",
            "value": 0.6764999
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "unit": "ms",
            "value": 1.025676
          },
          {
            "name": "interop/calls/mean positional",
            "unit": "ms",
            "value": 1.061182
          },
          {
            "name": "interop/calls/mean named",
            "unit": "ms",
            "value": 1.084204
          },
          {
            "name": "interop/calls/seq positional",
            "unit": "ms",
            "value": 0.467072
          },
          {
            "name": "interop/calls/seq named",
            "unit": "ms",
            "value": 0.533041
          },
          {
            "name": "interop/objects/vector",
            "unit": "ms",
            "value": 0.9383464
          },
          {
            "name": "interop/objects/list",
            "unit": "ms",
            "value": 0.9550541
          },
          {
            "name": "interop/objects/data.frame",
            "unit": "ms",
            "value": 1.034881
          },
          {
            "name": "interop/objects/formula",
            "unit": "ms",
            "value": 0.362481
          },
          {
            "name": "modules/import/import binding",
            "unit": "ms",
            "value": 78.98062
          },
          {
            "name": "modules/import/import control",
            "unit": "ms",
            "value": 86.78154
          },
          {
            "name": "modules/load_run/load",
            "unit": "ms",
            "value": 87.01936
          },
          {
            "name": "modules/load_run/run",
            "unit": "ms",
            "value": 83.66109
          },
          {
            "name": "e2e/synthetic/Micro",
            "unit": "ms",
            "value": 81.91663
          },
          {
            "name": "e2e/synthetic/Small",
            "unit": "ms",
            "value": 86.13407
          },
          {
            "name": "e2e/synthetic/Medium",
            "unit": "ms",
            "value": 156.5173
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "unit": "ms",
            "value": 97.87005
          },
          {
            "name": "e2e/strings/1K string",
            "unit": "ms",
            "value": 82.91551
          },
          {
            "name": "e2e/strings/10K string",
            "unit": "ms",
            "value": 79.61831
          },
          {
            "name": "e2e/args/50 args",
            "unit": "ms",
            "value": 86.15791
          },
          {
            "name": "e2e/args/100 args",
            "unit": "ms",
            "value": 89.79486
          },
          {
            "name": "e2e/repl/REPL session",
            "unit": "ms",
            "value": 79.8573
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "unit": "ms",
            "value": 253.2088
          },
          {
            "name": "e2e/real/quicksort.arl",
            "unit": "ms",
            "value": 286.8695
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "unit": "ms",
            "value": 196.5987
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "unit": "ms",
            "value": 290.7508
          }
        ]
      },
      {
        "commit": {
          "author": {
            "username": "William Brannon"
          },
          "committer": {
            "username": "William Brannon"
          },
          "id": "d858dfa",
          "message": "Add missing imports to examples and fix benchmark string ops",
          "timestamp": "2026-02-19T02:57:18Z",
          "url": "https://github.com/wwbrannon/arl/commit/d858dfa"
        },
        "date": 1771472487000,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "unit": "ms",
            "value": 0.0357111
          },
          {
            "name": "tokenizer/strings/100 chars",
            "unit": "ms",
            "value": 0.05026627
          },
          {
            "name": "tokenizer/strings/1K chars",
            "unit": "ms",
            "value": 0.1562512
          },
          {
            "name": "tokenizer/strings/10K chars",
            "unit": "ms",
            "value": 1.224711
          },
          {
            "name": "tokenizer/nested/10 levels",
            "unit": "ms",
            "value": 0.1048371
          },
          {
            "name": "tokenizer/nested/50 levels",
            "unit": "ms",
            "value": 0.2292721
          },
          {
            "name": "tokenizer/nested/100 levels",
            "unit": "ms",
            "value": 0.3853999
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "unit": "ms",
            "value": 0.4463466
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "unit": "ms",
            "value": 4.235833
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "unit": "ms",
            "value": 42.14659
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "unit": "ms",
            "value": 0.03579259
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "unit": "ms",
            "value": 0.03554719
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "unit": "ms",
            "value": 0.09639096
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "unit": "ms",
            "value": 16.78786
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "unit": "ms",
            "value": 25.27832
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "unit": "ms",
            "value": 18.0022
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "unit": "ms",
            "value": 18.7279
          },
          {
            "name": "parser/flat/10 elements",
            "unit": "ms",
            "value": 0.132143
          },
          {
            "name": "parser/flat/100 elements",
            "unit": "ms",
            "value": 1.030699
          },
          {
            "name": "parser/flat/1000 elements",
            "unit": "ms",
            "value": 10.0721
          },
          {
            "name": "parser/nested/Depth 5",
            "unit": "ms",
            "value": 0.1550624
          },
          {
            "name": "parser/nested/Depth 10",
            "unit": "ms",
            "value": 0.2957331
          },
          {
            "name": "parser/nested/Depth 20",
            "unit": "ms",
            "value": 0.5826307
          },
          {
            "name": "parser/sugar/Single quote expr",
            "unit": "ms",
            "value": 0.1780628
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "unit": "ms",
            "value": 1.773968
          },
          {
            "name": "parser/nulls/Few NULLs",
            "unit": "ms",
            "value": 0.09331619
          },
          {
            "name": "parser/nulls/100 NULLs",
            "unit": "ms",
            "value": 1.049846
          },
          {
            "name": "parser/real/fibonacci.arl",
            "unit": "ms",
            "value": 7.147243
          },
          {
            "name": "parser/real/quicksort.arl",
            "unit": "ms",
            "value": 10.32208
          },
          {
            "name": "parser/real/graph-paths.arl",
            "unit": "ms",
            "value": 8.244731
          },
          {
            "name": "parser/real/macro-examples.arl",
            "unit": "ms",
            "value": 9.180597
          },
          {
            "name": "macro/simple/Simple macro",
            "unit": "ms",
            "value": 0.5097119
          },
          {
            "name": "macro/complex/Complex macro",
            "unit": "ms",
            "value": 0.8358057
          },
          {
            "name": "macro/nested/Nested macros",
            "unit": "ms",
            "value": 1.791208
          },
          {
            "name": "macro/hygiene/With hygiene",
            "unit": "ms",
            "value": 1.069567
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "unit": "ms",
            "value": 5.370098
          },
          {
            "name": "macro/real/fibonacci.arl",
            "unit": "ms",
            "value": 71.52587
          },
          {
            "name": "macro/real/quicksort.arl",
            "unit": "ms",
            "value": 100.107
          },
          {
            "name": "macro/real/graph-paths.arl",
            "unit": "ms",
            "value": 29.28573
          },
          {
            "name": "macro/real/macro-examples.arl",
            "unit": "ms",
            "value": 35.43948
          },
          {
            "name": "compile/arithmetic/Single add",
            "unit": "ms",
            "value": 0.05998276
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.1460623
          },
          {
            "name": "compile/arithmetic/Many adds",
            "unit": "ms",
            "value": 0.4055719
          },
          {
            "name": "compile/calls/1 arg",
            "unit": "ms",
            "value": 0.04546903
          },
          {
            "name": "compile/calls/5 args",
            "unit": "ms",
            "value": 0.08105719
          },
          {
            "name": "compile/calls/10 args",
            "unit": "ms",
            "value": 0.1261979
          },
          {
            "name": "compile/special/if",
            "unit": "ms",
            "value": 0.05592406
          },
          {
            "name": "compile/special/define",
            "unit": "ms",
            "value": 0.05707238
          },
          {
            "name": "compile/special/lambda",
            "unit": "ms",
            "value": 0.09274203
          },
          {
            "name": "compile/special/begin",
            "unit": "ms",
            "value": 0.05309517
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 0.04526367
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 0.04432071
          },
          {
            "name": "compile/recursive/factorial(100)",
            "unit": "ms",
            "value": 0.04497729
          },
          {
            "name": "compile/recursive/factorial(500)",
            "unit": "ms",
            "value": 0.044649
          },
          {
            "name": "compile/closures/Create closure",
            "unit": "ms",
            "value": 0.05928613
          },
          {
            "name": "compile/real/fibonacci.arl",
            "unit": "ms",
            "value": 8.5
          },
          {
            "name": "compile/real/quicksort.arl",
            "unit": "ms",
            "value": 12
          },
          {
            "name": "compile/real/graph-paths.arl",
            "unit": "ms",
            "value": 25
          },
          {
            "name": "compile/real/macro-examples.arl",
            "unit": "ms",
            "value": 28
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "unit": "ms",
            "value": 0.01525227
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "unit": "ms",
            "value": 0.01533376
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "unit": "ms",
            "value": 0.02706004
          },
          {
            "name": "r-eval/calls/1 arg",
            "unit": "ms",
            "value": 0.01484202
          },
          {
            "name": "r-eval/calls/5 args",
            "unit": "ms",
            "value": 0.0264449
          },
          {
            "name": "r-eval/calls/10 args",
            "unit": "ms",
            "value": 0.04005712
          },
          {
            "name": "r-eval/special/if",
            "unit": "ms",
            "value": 0.01394004
          },
          {
            "name": "r-eval/special/define",
            "unit": "ms",
            "value": 0.01722015
          },
          {
            "name": "r-eval/special/lambda",
            "unit": "ms",
            "value": 0.0159489
          },
          {
            "name": "r-eval/special/begin",
            "unit": "ms",
            "value": 0.01406344
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "unit": "ms",
            "value": 0.9430617
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "unit": "ms",
            "value": 2.44524
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "unit": "ms",
            "value": 0.7414031
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "unit": "ms",
            "value": 3.640759
          },
          {
            "name": "r-eval/closures/Create closure",
            "unit": "ms",
            "value": 0.02152519
          },
          {
            "name": "r-eval/closures/Call closure",
            "unit": "ms",
            "value": 0.03234902
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "unit": "ms",
            "value": 71
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "unit": "ms",
            "value": 85
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "unit": "ms",
            "value": 61
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "unit": "ms",
            "value": 82
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "unit": "ms",
            "value": 0.3146753
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "unit": "ms",
            "value": 0.3264213
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "unit": "ms",
            "value": 0.3150029
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "unit": "ms",
            "value": 0.3193694
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "unit": "ms",
            "value": 0.3167456
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "unit": "ms",
            "value": 0.3337401
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "unit": "ms",
            "value": 0.4166625
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "unit": "ms",
            "value": 0.4371831
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "unit": "ms",
            "value": 0.8042972
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "unit": "ms",
            "value": 4.51863
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "unit": "ms",
            "value": 0.4257851
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "unit": "ms",
            "value": 0.6601
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "unit": "ms",
            "value": 0.5004667
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "unit": "ms",
            "value": 0.797614
          },
          {
            "name": "stdlib/compose/Single function",
            "unit": "ms",
            "value": 0.3293531
          },
          {
            "name": "stdlib/compose/Two composed",
            "unit": "ms",
            "value": 0.4461622
          },
          {
            "name": "stdlib/compose/Three composed",
            "unit": "ms",
            "value": 0.5604702
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "unit": "ms",
            "value": 0.3517801
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "unit": "ms",
            "value": 0.4488272
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "unit": "ms",
            "value": 0.6301289
          },
          {
            "name": "stdlib/predicates/null?",
            "unit": "ms",
            "value": 0.270969
          },
          {
            "name": "stdlib/predicates/list?",
            "unit": "ms",
            "value": 0.5788379
          },
          {
            "name": "stdlib/predicates/number?",
            "unit": "ms",
            "value": 0.3209072
          },
          {
            "name": "stdlib/predicates/string?",
            "unit": "ms",
            "value": 0.2890707
          },
          {
            "name": "stdlib/construction/cons (single)",
            "unit": "ms",
            "value": 0.5826307
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "unit": "ms",
            "value": 0.8180731
          },
          {
            "name": "stdlib/construction/append (large)",
            "unit": "ms",
            "value": 0.9839181
          },
          {
            "name": "stdlib/construction/range (10)",
            "unit": "ms",
            "value": 0.4480686
          },
          {
            "name": "stdlib/construction/range (100)",
            "unit": "ms",
            "value": 0.542348
          },
          {
            "name": "stdlib/nested/map then filter",
            "unit": "ms",
            "value": 0.6658605
          },
          {
            "name": "stdlib/nested/filter then map",
            "unit": "ms",
            "value": 0.6495425
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "unit": "ms",
            "value": 0.9544184
          },
          {
            "name": "interop/calls/mean positional",
            "unit": "ms",
            "value": 1.073728
          },
          {
            "name": "interop/calls/mean named",
            "unit": "ms",
            "value": 1.103044
          },
          {
            "name": "interop/calls/seq positional",
            "unit": "ms",
            "value": 0.488597
          },
          {
            "name": "interop/calls/seq named",
            "unit": "ms",
            "value": 0.5500149
          },
          {
            "name": "interop/objects/vector",
            "unit": "ms",
            "value": 0.9553409
          },
          {
            "name": "interop/objects/list",
            "unit": "ms",
            "value": 0.9609582
          },
          {
            "name": "interop/objects/data.frame",
            "unit": "ms",
            "value": 1.064647
          },
          {
            "name": "interop/objects/formula",
            "unit": "ms",
            "value": 0.4017181
          },
          {
            "name": "modules/import/import binding",
            "unit": "ms",
            "value": 133.7227
          },
          {
            "name": "modules/import/import control",
            "unit": "ms",
            "value": 149.8729
          },
          {
            "name": "modules/load_run/load",
            "unit": "ms",
            "value": 146.9071
          },
          {
            "name": "modules/load_run/run",
            "unit": "ms",
            "value": 144.3048
          },
          {
            "name": "e2e/synthetic/Micro",
            "unit": "ms",
            "value": 158.9605
          },
          {
            "name": "e2e/synthetic/Small",
            "unit": "ms",
            "value": 130.0033
          },
          {
            "name": "e2e/synthetic/Medium",
            "unit": "ms",
            "value": 219.4662
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "unit": "ms",
            "value": 161.9325
          },
          {
            "name": "e2e/strings/1K string",
            "unit": "ms",
            "value": 150.0846
          },
          {
            "name": "e2e/strings/10K string",
            "unit": "ms",
            "value": 141.1193
          },
          {
            "name": "e2e/args/50 args",
            "unit": "ms",
            "value": 171.0478
          },
          {
            "name": "e2e/args/100 args",
            "unit": "ms",
            "value": 162.6197
          },
          {
            "name": "e2e/repl/REPL session",
            "unit": "ms",
            "value": 138.2831
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "unit": "ms",
            "value": 304.0734
          },
          {
            "name": "e2e/real/quicksort.arl",
            "unit": "ms",
            "value": 385.0525
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "unit": "ms",
            "value": 333.0463
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "unit": "ms",
            "value": 354.3437
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "9ca24ae1298f150ad063dc19623f17a4b91707c1",
          "message": "Fix CI failures across all three default workflows\n\n- Add devtools to coverage and benchmarks workflow dependencies\n- Replace %||% with if/else in module-cache.R for R < 4.4 compat\n- Normalize file paths in CoverageTracker for Windows backslash handling\n- Fix get_summary() to split on last colon (Windows drive letters)\n- Fix directory-list test to accept Windows drive-letter paths\n\nCo-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>",
          "timestamp": "2026-02-19T17:14:06Z",
          "tree_id": "e588e28dd10d858a8146e608119f71ff0c9cf4a2",
          "url": "https://github.com/wwbrannon/arl/commit/9ca24ae1298f150ad063dc19623f17a4b91707c1"
        },
        "date": 1771521858572,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.04552607,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.06691611,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.2562971,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.384586,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08139305,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.2618721,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4853066,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.2016891,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.683171,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.2896,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04451408,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.0453851,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.1142646,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 5.01327,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.599161,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.593627,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.815491,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.2032621,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.589475,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 15.72789,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.2330581,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4477156,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.874302,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2726821,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.687265,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.1446321,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.577451,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.71583,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.52025,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.34778,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 13.97721,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.8286266,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.338304,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 2.959116,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.760797,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.578079,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 91.48536,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 126.0305,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 22.07177,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 30.33341,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.08805108,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2147231,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6025165,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.06475655,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1190091,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1842261,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08115708,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.08492009,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1496311,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.07777108,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.06838358,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.06513257,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.06842404,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.06486656,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.05360105,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 13.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 18,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 15.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 27.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.02238702,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.02300355,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.03724004,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.02388511,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.0732725,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.07201557,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.02260756,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.04490954,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.02776756,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.02348953,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 1.8318,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 4.596036,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 2.016858,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 9.36951,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.03088801,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.04849106,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 115,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 135.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 104,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 166.5,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.3733315,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.3927525,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.3751201,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.3780556,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.3755651,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.4063231,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.5368285,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.5168706,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.118391,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 7.053935,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.5015166,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.9112356,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.57506,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.0757,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.4187316,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.537204,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6507071,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.5032656,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6604051,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9136156,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3735971,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6324381,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.4102705,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.3980426,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.643303,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.8858831,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.150265,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.560337,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.6909221,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.7129486,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.6823911,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.9600476,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.027439,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.067559,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.5846831,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.689254,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.9019431,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.9176026,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.174334,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.44759,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 194.8873,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 198.6889,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 200.2782,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 204.3466,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 176.3509,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 218.481,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 224.6024,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 187.1043,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 185.5296,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 209.7796,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 194.4022,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 204.7213,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 206.4208,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 544.6411,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 458.7212,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 357.6149,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 664.3367,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "173589a3bfd30bac6d2b50998c9e6e7485c7b98e",
          "message": "Fix norm_path to resolve symlinks for non-existent files\n\nOn macOS, normalizePath only resolves /var -> /private/var when the\nfile exists. Since tests call norm_path before writeLines, normalize\nthe parent directory (which exists) and append the basename.\n\nCo-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>",
          "timestamp": "2026-02-19T18:16:16Z",
          "tree_id": "64fd0593b27f18b40697771b29d1470f4ae2b909",
          "url": "https://github.com/wwbrannon/arl/commit/173589a3bfd30bac6d2b50998c9e6e7485c7b98e"
        },
        "date": 1771526182697,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.04659605,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.06689411,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.2580391,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.373228,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08175103,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.2608286,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4832616,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.19951,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.671539,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.31326,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04492304,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04573504,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.1146631,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 4.946763,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.551172,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.581916,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.79718,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.202255,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.576389,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 15.56363,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.2333031,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.445556,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.866437,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2713031,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.686829,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.1428751,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.573618,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.7028,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.46795,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.34407,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 13.92207,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.836331,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.352524,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 2.984977,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.762249,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.696119,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 93.81252,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 128.5158,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 21.90393,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 31.34581,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.0901671,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2181795,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6035551,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.06399408,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1184501,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1819574,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08083007,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.0853985,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1498431,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.0752901,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.0642091,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.06694498,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.06463006,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.06770052,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.05402509,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 15,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 18.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 15.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 14.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.03776007,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.02464053,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.02315309,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.02467103,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.07281511,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.107169,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.03772008,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.02951006,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.02686511,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.03805105,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 2.987271,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 4.958006,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 2.259251,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 10.69411,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.03113807,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.05001255,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 104,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 121,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 89,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 148.5,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.3711336,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.3943761,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.3781621,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.3776806,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.3788981,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.4059426,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.5359185,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.5146646,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.106718,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 7.028204,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.5001281,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.9040916,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.5796901,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.734507,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.4158315,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.5358536,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6475911,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.5004631,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6582461,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9120865,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3766335,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6348626,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.4092845,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.4007836,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6404885,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.894163,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.147468,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5584206,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.690937,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.7091051,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.6833231,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.9625451,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.036683,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.071552,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.5881761,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.6906711,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.9016619,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.922275,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.1707,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.4482261,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 259.6345,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 255.4106,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 250.567,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 266.0209,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 275.5572,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 279.5953,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 283.3694,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 259.6573,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 268.5144,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 242.4043,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 269.9688,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 259.238,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 275.9197,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 587.2681,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 481.7765,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 402.705,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 688.3849,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "bcc35b3a1d0c994e345ac9984c150603cbb00aa5",
          "message": "Add disable toggles for 6 compiler optimizations and disable_optimizations master switch\n\nConstant folding previously masked a runtime bug (variadic_eq NaN handling).\nThe same pattern applies to other optimizations that bypass runtime code paths.\nAdd individual enable_* fields for dead code elimination, strength reduction,\nidentity elimination, truthiness optimization, begin simplification, and\nboolean flattening. Add disable_optimizations Engine parameter (with R option\nand env var support) that turns all optimizations off at once for testing.\n\nCo-Authored-By: Claude Opus 4.6 <noreply@anthropic.com>",
          "timestamp": "2026-02-20T12:53:28-05:00",
          "tree_id": "3a2bf6ca8c688fac8f8ce16346c175bf9b0edb21",
          "url": "https://github.com/wwbrannon/arl/commit/bcc35b3a1d0c994e345ac9984c150603cbb00aa5"
        },
        "date": 1771611938267,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.05544408,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.07757958,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.2599951,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.374762,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08374604,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.2684201,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4945011,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.2038651,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.681985,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.02101,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04623609,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04737359,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.1001215,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 4.90468,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.489317,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.510787,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.715982,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.2097861,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.615361,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 16.10017,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.2384495,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4544971,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.8907301,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2772521,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.729302,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.1468387,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.597007,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.94377,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.69533,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.42971,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 14.11719,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.8430511,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.359284,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 2.987588,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.779797,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.933283,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 93.06552,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 131.2674,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 21.89012,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 30.95639,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.09045907,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2198951,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6136536,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.07453404,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1241561,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1858716,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08249859,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.08876051,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1551791,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.07939304,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.06946007,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.07326656,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.06981008,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.07207959,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.09624963,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 16,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 17.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 33,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 16.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.03945804,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.02527208,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.02400007,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.02523698,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.0557091,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.07772504,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.02415507,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.03095204,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.04508963,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.02570852,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 2.950798,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 5.694141,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 2.228021,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 11.15714,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.06269157,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.06212055,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 104,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 122.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 92.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 147,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.6288771,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.6539841,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.650447,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.3980566,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.3971701,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.4325116,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.5603341,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.5245426,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.126194,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 7.082734,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.5115186,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.9419356,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.5874446,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.091489,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.4318751,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.5491686,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6655706,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.516923,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6714616,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9275235,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3850679,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6513941,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.421671,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.411412,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6676046,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.9088945,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.168047,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5734744,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.7067175,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.7286681,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.6978911,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.981304,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.047813,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.08646,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.5988061,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.6995185,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.9179361,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.9377431,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.211363,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.460894,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 211.0345,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 214.3522,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 217.2077,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 223.675,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 184.8979,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 238.0791,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 233.1997,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 206.8311,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 214.7677,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 216.605,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 219.3671,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 213.0191,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 209.4394,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 478.7542,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 600.2644,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 301.0615,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 539.3541,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "4f362864d9bd06ba1060d9eb391395cf75199e5e",
          "message": "Merge pull request #1 from wwbrannon/dependabot/github_actions/actions/upload-artifact-6\n\nBump actions/upload-artifact from 4 to 6",
          "timestamp": "2026-02-20T13:50:46-05:00",
          "tree_id": "14f2276ae0421628570b626c0e744753320b88e8",
          "url": "https://github.com/wwbrannon/arl/commit/4f362864d9bd06ba1060d9eb391395cf75199e5e"
        },
        "date": 1771614055366,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.04670804,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.06779714,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.2586526,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.378233,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08327607,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.2683511,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4935361,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.2045766,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.703249,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.62315,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04511408,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04599511,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.1141221,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 5.017805,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.677509,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.674845,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.891406,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.2060101,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.601189,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 15.66241,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.2374841,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4524551,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.8828581,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2748941,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.709958,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.1457421,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.597302,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.7524,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.66436,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.4218,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 14.12721,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.8422771,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.356778,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 3.014697,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.810845,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.891688,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 95.04891,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 129.8734,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 21.99898,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 31.06837,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.09018357,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2197251,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6098431,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.06961502,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1246226,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1879256,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.0812519,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.08405256,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1549895,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.08106156,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.06909954,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.07108296,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.06859808,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.07368758,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.06024307,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 22.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 25.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 37.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 31.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.02347404,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.0367086,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.03787555,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.0407811,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.05068013,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.1184411,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.02532208,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.02951507,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.04295504,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.03773108,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 1.50292,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 3.957566,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 1.202504,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 5.901619,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.03172888,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.05183701,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 129,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 166.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 113.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 105.5,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.4024211,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.4219381,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.4039241,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.4062891,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.4046506,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.434321,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.568581,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.565846,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.169158,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 7.28743,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.529258,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.9652666,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.6024045,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.123166,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.4423305,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.5737606,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6928231,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.5365461,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6945111,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9636136,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3996461,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6752301,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.4360335,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.4252735,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6823785,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.9290436,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.206963,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5880731,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.7223231,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.751017,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.7151451,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.9826635,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.040703,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.08493,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.603907,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.7080921,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.9268445,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.947348,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.221996,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.4671265,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 202.4881,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 214.4628,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 218.9748,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 204.4793,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 222.6911,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 189.709,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 380.7119,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 244.0333,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 199.656,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 195.4098,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 221.9792,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 231.6518,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 265.906,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 607.9037,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 732.1448,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 434.9802,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 606.4419,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "0ef6dbc887908b31fc7bb29c1307c82f00771fab",
          "message": "Merge remote-tracking branch 'origin/main'\n\n* origin/main:\n  Bump actions/checkout from 4 to 6\n  Bump actions/upload-artifact from 4 to 6\n  Bump actions/github-script from 7 to 8",
          "timestamp": "2026-02-20T15:43:40-05:00",
          "tree_id": "0a0afb4095071208553b57841b185a042255f3e9",
          "url": "https://github.com/wwbrannon/arl/commit/0ef6dbc887908b31fc7bb29c1307c82f00771fab"
        },
        "date": 1771620777428,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.04702806,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.06759609,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.258283,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.371709,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08235406,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.259801,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4825321,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.2022391,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.672477,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.12749,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04522409,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04611607,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.116348,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 4.955488,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.539053,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.589635,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.768763,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.2028291,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.574443,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 15.55513,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.231442,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4405036,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.864175,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.269806,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.671497,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.1426871,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.566874,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.56732,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.3222,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.15963,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 13.77616,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.8190711,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.327677,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 2.9426,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.778425,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.670653,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 91.7709,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 126.2835,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 21.59159,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 30.77724,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.08945743,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2221751,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6105456,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.07072208,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1259255,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1905159,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08040556,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.08382712,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1526956,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.07774011,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.06985007,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.06915902,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.07118809,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.06893405,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.05923107,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 14,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 19.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 22.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 14,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.02199213,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.03264652,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.03479014,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.02436509,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.07013656,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.07237005,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.02290757,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.02806803,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.04006457,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.02422009,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 1.774456,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 7.09629,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 1.363979,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 8.240498,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.05494809,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.05757809,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 139,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 107.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 78.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 127,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.5229376,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.4761295,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.6079455,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.4665866,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.5929985,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.6529755,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.6496795,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.6220226,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.897381,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 10.13032,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.5140116,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.9404576,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.5862306,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.082193,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.424163,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.5458106,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.658671,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.5086155,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6647881,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9185016,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.379179,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6432571,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.418267,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.408208,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6527096,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.8909251,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.154994,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5640291,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.696757,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.7237921,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.6954391,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.9721266,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.03545,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.073866,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.589552,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.6926895,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.90972,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.929377,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.201758,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.4653891,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 208.5494,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 217.5229,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 205.0862,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 207.9913,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 182.2901,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 228.5261,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 212.3097,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 225.1772,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 212.9913,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 198.203,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 218.2565,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 216.5066,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 196.9482,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 430.6787,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 529.7233,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 294.1236,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 495.622,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "2ebf5489f1765368a084c652ef95cbb398ef77dd",
          "message": "fix codecov upload configuration",
          "timestamp": "2026-02-20T16:45:17-05:00",
          "tree_id": "bcca4e6d174859ff6964e73eb69e521dbdd1fe1e",
          "url": "https://github.com/wwbrannon/arl/commit/2ebf5489f1765368a084c652ef95cbb398ef77dd"
        },
        "date": 1771624463370,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.04683807,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.06713608,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.2591331,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.39377,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08319505,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.2634409,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4880001,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.201315,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.669173,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.39219,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04626706,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04736811,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.09914511,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 5.025567,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.644451,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.661584,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.867878,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.2059251,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.60844,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 16.28514,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.2360256,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4505406,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.882771,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2757251,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.712679,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.144781,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.582161,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.94648,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.85969,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.64667,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 14.2227,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.8427121,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.355419,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 3.02761,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.823315,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.862657,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 91.49977,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 129.7766,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 22.37,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 31.15645,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.08838956,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.219509,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6048931,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.0713981,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1215971,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1846596,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08433254,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.08455309,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1551191,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.07786509,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.07174409,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.07047155,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.06950961,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.07126806,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.0865066,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 15,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 22,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 13,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 16,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.03587146,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.03637816,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.02282759,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.03792113,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.04621159,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.09941554,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.03703457,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.02836308,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.02608856,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.03360806,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 1.786987,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 7.409161,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 1.321084,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 6.721574,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.05683111,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.05879003,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 153.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 113,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 81.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 140.5,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.451802,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.6354096,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.4332281,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.5988016,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.4344701,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.6410601,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.6462151,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.8598281,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 2.047508,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 12.54839,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.5680191,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 1.58479,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.6758345,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.85592,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.5027021,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.5528761,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6678111,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.510472,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6733556,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9237376,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3801491,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6479681,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.4180596,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.407019,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6502431,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.9024476,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.172011,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5657896,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.6996346,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.7234986,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.6894451,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.9788606,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.049873,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.090564,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.5983011,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.7036671,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.9226505,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.9443261,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.221102,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.4630031,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 212.0171,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 205.3873,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 208.7962,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 217.2057,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 188.7975,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 238.1426,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 216.3577,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 229.756,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 223.6005,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 209.6909,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 220.5449,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 213.5993,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 201.5338,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 452.7015,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 568.8694,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 284.7179,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 504.5993,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "36ae4dbb446703cf3b8e27fdc2d4f940f251ce28",
          "message": "fix typo",
          "timestamp": "2026-02-22T23:19:43-08:00",
          "tree_id": "93a93705fc18417e19bacb8af7a83791aa1c9c58",
          "url": "https://github.com/wwbrannon/arl/commit/36ae4dbb446703cf3b8e27fdc2d4f940f251ce28"
        },
        "date": 1771834003151,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.05471217,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.06703509,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.2613265,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.379497,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.08374616,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.2673781,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4941446,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.2101411,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.761052,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 17.25853,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04845113,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04970212,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.11841,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 5.028887,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.643146,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.631858,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.75118,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.2045361,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.586877,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 15.96653,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.235478,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4505682,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.8818656,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2761341,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.690837,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.1449001,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.587093,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.75194,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.63347,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.39355,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 13.99593,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.7974191,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.331062,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 2.809898,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.747772,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.58707,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 92.84526,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 129.1584,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 21.93584,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 31.20259,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.0897671,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2258451,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.6082666,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.07615204,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1268111,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1939505,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08689659,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.08936162,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1731121,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.07893605,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.07384806,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.07602159,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.07162412,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.07670757,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.06101909,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 15,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 19.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 31,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 35,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.03506499,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.02222206,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.02202106,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.02239703,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.06716,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.07009611,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.0223415,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.04548009,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.02627901,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.02249208,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 1.422687,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 3.69759,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 1.201981,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 5.914218,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.03137905,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.05010358,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 257.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 293.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 225,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 287,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.387852,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.4082646,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.3906625,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.3896196,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.3901771,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.417928,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.5470075,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.5259885,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.083667,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 6.485773,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.5101951,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.8826176,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.5788021,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 1.02356,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.4320891,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.555228,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6705581,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.5113416,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.6631896,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.9201681,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3865051,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6544031,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.4246901,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.415643,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6690201,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.84923,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.117459,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5721191,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.7075961,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.721362,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.694046,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.972369,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 1.04232,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.073057,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.587052,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.6884011,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.9134896,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.932571,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.205714,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.458899,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 437.9422,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 448.4218,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 436.9754,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 445.9106,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 430.1983,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 461.986,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 508.4508,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 465.4347,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 434.0331,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 445.2283,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 447.4914,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 448.2704,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 439.3483,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 790.4539,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 907.0508,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 753.8627,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 1032.55,
            "unit": "ms"
          }
        ]
      },
      {
        "commit": {
          "author": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "committer": {
            "email": "will.brannon@gmail.com",
            "name": "William Brannon",
            "username": "wwbrannon"
          },
          "distinct": true,
          "id": "cfe6a583782de7cd132d57a2ffb35469053af5a6",
          "message": "update READMEs",
          "timestamp": "2026-02-23T01:21:37-08:00",
          "tree_id": "bc27b040d34c1e6b759a5f51833897806bf0bb5e",
          "url": "https://github.com/wwbrannon/arl/commit/cfe6a583782de7cd132d57a2ffb35469053af5a6"
        },
        "date": 1771839621159,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "tokenizer/strings/10 chars",
            "value": 0.04855404,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/100 chars",
            "value": 0.05631614,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/1K chars",
            "value": 0.1657261,
            "unit": "ms"
          },
          {
            "name": "tokenizer/strings/10K chars",
            "value": 1.229205,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/10 levels",
            "value": 0.07737707,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/50 levels",
            "value": 0.262787,
            "unit": "ms"
          },
          {
            "name": "tokenizer/nested/100 levels",
            "value": 0.4947181,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Small (3 exprs)",
            "value": 0.1943839,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Medium (30 exprs)",
            "value": 1.708705,
            "unit": "ms"
          },
          {
            "name": "tokenizer/mixed/Large (300 exprs)",
            "value": 16.75213,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/No escapes",
            "value": 0.04009099,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Few escapes",
            "value": 0.04090212,
            "unit": "ms"
          },
          {
            "name": "tokenizer/escapes/Many escapes",
            "value": 0.09163807,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/fibonacci.arl",
            "value": 5.108438,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/quicksort.arl",
            "value": 7.824342,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/graph-paths.arl",
            "value": 4.549663,
            "unit": "ms"
          },
          {
            "name": "tokenizer/real/macro-examples.arl",
            "value": 5.724184,
            "unit": "ms"
          },
          {
            "name": "parser/flat/10 elements",
            "value": 0.195747,
            "unit": "ms"
          },
          {
            "name": "parser/flat/100 elements",
            "value": 1.566981,
            "unit": "ms"
          },
          {
            "name": "parser/flat/1000 elements",
            "value": 15.89906,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 5",
            "value": 0.2231781,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 10",
            "value": 0.4338661,
            "unit": "ms"
          },
          {
            "name": "parser/nested/Depth 20",
            "value": 0.8564855,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/Single quote expr",
            "value": 0.2599231,
            "unit": "ms"
          },
          {
            "name": "parser/sugar/10 quote exprs",
            "value": 2.605511,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/Few NULLs",
            "value": 0.137718,
            "unit": "ms"
          },
          {
            "name": "parser/nulls/100 NULLs",
            "value": 1.561461,
            "unit": "ms"
          },
          {
            "name": "parser/real/fibonacci.arl",
            "value": 10.5275,
            "unit": "ms"
          },
          {
            "name": "parser/real/quicksort.arl",
            "value": 15.26792,
            "unit": "ms"
          },
          {
            "name": "parser/real/graph-paths.arl",
            "value": 12.19907,
            "unit": "ms"
          },
          {
            "name": "parser/real/macro-examples.arl",
            "value": 13.73613,
            "unit": "ms"
          },
          {
            "name": "macro/simple/Simple macro",
            "value": 0.709623,
            "unit": "ms"
          },
          {
            "name": "macro/complex/Complex macro",
            "value": 1.196134,
            "unit": "ms"
          },
          {
            "name": "macro/nested/Nested macros",
            "value": 2.523868,
            "unit": "ms"
          },
          {
            "name": "macro/hygiene/With hygiene",
            "value": 1.591783,
            "unit": "ms"
          },
          {
            "name": "macro/heavy/Macro-heavy",
            "value": 8.05207,
            "unit": "ms"
          },
          {
            "name": "macro/real/fibonacci.arl",
            "value": 93.10912,
            "unit": "ms"
          },
          {
            "name": "macro/real/quicksort.arl",
            "value": 125.9339,
            "unit": "ms"
          },
          {
            "name": "macro/real/graph-paths.arl",
            "value": 21.96447,
            "unit": "ms"
          },
          {
            "name": "macro/real/macro-examples.arl",
            "value": 30.87915,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Single add",
            "value": 0.08481857,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Nested adds",
            "value": 0.2041386,
            "unit": "ms"
          },
          {
            "name": "compile/arithmetic/Many adds",
            "value": 0.564414,
            "unit": "ms"
          },
          {
            "name": "compile/calls/1 arg",
            "value": 0.06959512,
            "unit": "ms"
          },
          {
            "name": "compile/calls/5 args",
            "value": 0.1171721,
            "unit": "ms"
          },
          {
            "name": "compile/calls/10 args",
            "value": 0.1782094,
            "unit": "ms"
          },
          {
            "name": "compile/special/if",
            "value": 0.08285604,
            "unit": "ms"
          },
          {
            "name": "compile/special/define",
            "value": 0.07853907,
            "unit": "ms"
          },
          {
            "name": "compile/special/lambda",
            "value": 0.1491401,
            "unit": "ms"
          },
          {
            "name": "compile/special/begin",
            "value": 0.07571955,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(10)",
            "value": 0.06825296,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/fibonacci(12)",
            "value": 0.065259,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(100)",
            "value": 0.06397202,
            "unit": "ms"
          },
          {
            "name": "compile/recursive/factorial(500)",
            "value": 0.07071707,
            "unit": "ms"
          },
          {
            "name": "compile/closures/Create closure",
            "value": 0.05522347,
            "unit": "ms"
          },
          {
            "name": "compile/real/fibonacci.arl",
            "value": 83.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/quicksort.arl",
            "value": 68.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/graph-paths.arl",
            "value": 58.5,
            "unit": "ms"
          },
          {
            "name": "compile/real/macro-examples.arl",
            "value": 16,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Single add",
            "value": 0.02117758,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Nested adds",
            "value": 0.02191798,
            "unit": "ms"
          },
          {
            "name": "r-eval/arithmetic/Many adds",
            "value": 0.03888906,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/1 arg",
            "value": 0.02663996,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/5 args",
            "value": 0.04544907,
            "unit": "ms"
          },
          {
            "name": "r-eval/calls/10 args",
            "value": 0.07070712,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/if",
            "value": 0.01922902,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/define",
            "value": 0.02447702,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/lambda",
            "value": 0.02173311,
            "unit": "ms"
          },
          {
            "name": "r-eval/special/begin",
            "value": 0.01930905,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(10)",
            "value": 1.393207,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/fibonacci(12)",
            "value": 3.647422,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(100)",
            "value": 1.180375,
            "unit": "ms"
          },
          {
            "name": "r-eval/recursive/factorial(500)",
            "value": 5.814018,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Create closure",
            "value": 0.02876343,
            "unit": "ms"
          },
          {
            "name": "r-eval/closures/Call closure",
            "value": 0.04329602,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/fibonacci.arl",
            "value": 255.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/quicksort.arl",
            "value": 286,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/graph-paths.arl",
            "value": 232.5,
            "unit": "ms"
          },
          {
            "name": "r-eval/real/macro-examples.arl",
            "value": 296.5,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/car (10)",
            "value": 0.339394,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/cdr (10)",
            "value": 0.3560984,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (10)",
            "value": 0.3437901,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (100)",
            "value": 0.3425235,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/length (1000)",
            "value": 0.3409961,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (10)",
            "value": 0.3669501,
            "unit": "ms"
          },
          {
            "name": "stdlib/list_ops/reverse (100)",
            "value": 0.4949686,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (10)",
            "value": 0.4687846,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (100)",
            "value": 1.038996,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/map (1000)",
            "value": 6.830181,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (10)",
            "value": 0.456906,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/filter (100)",
            "value": 0.8448326,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (10)",
            "value": 0.5302075,
            "unit": "ms"
          },
          {
            "name": "stdlib/higher_order/reduce (100)",
            "value": 0.9883251,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Single function",
            "value": 0.380746,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Two composed",
            "value": 0.4972121,
            "unit": "ms"
          },
          {
            "name": "stdlib/compose/Three composed",
            "value": 0.6158016,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (2 args)",
            "value": 0.4545876,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (5 args)",
            "value": 0.612347,
            "unit": "ms"
          },
          {
            "name": "stdlib/strings/string-concat (10 args)",
            "value": 0.870046,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/null?",
            "value": 0.3381765,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/list?",
            "value": 0.6039841,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/number?",
            "value": 0.3733051,
            "unit": "ms"
          },
          {
            "name": "stdlib/predicates/string?",
            "value": 0.3598694,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/cons (single)",
            "value": 0.6206141,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (2 lists)",
            "value": 0.7901556,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/append (large)",
            "value": 1.059683,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (10)",
            "value": 0.5192106,
            "unit": "ms"
          },
          {
            "name": "stdlib/construction/range (100)",
            "value": 0.6640744,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map then filter",
            "value": 0.65805,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/filter then map",
            "value": 0.6309194,
            "unit": "ms"
          },
          {
            "name": "stdlib/nested/map, filter, reduce",
            "value": 0.9102165,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean positional",
            "value": 0.989272,
            "unit": "ms"
          },
          {
            "name": "interop/calls/mean named",
            "value": 1.030719,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq positional",
            "value": 0.5390206,
            "unit": "ms"
          },
          {
            "name": "interop/calls/seq named",
            "value": 0.6359621,
            "unit": "ms"
          },
          {
            "name": "interop/objects/vector",
            "value": 0.8611726,
            "unit": "ms"
          },
          {
            "name": "interop/objects/list",
            "value": 0.8813936,
            "unit": "ms"
          },
          {
            "name": "interop/objects/data.frame",
            "value": 1.127706,
            "unit": "ms"
          },
          {
            "name": "interop/objects/formula",
            "value": 0.40342,
            "unit": "ms"
          },
          {
            "name": "modules/import/import binding",
            "value": 435.5177,
            "unit": "ms"
          },
          {
            "name": "modules/import/import control",
            "value": 454.9534,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/load",
            "value": 435.3409,
            "unit": "ms"
          },
          {
            "name": "modules/load_run/run",
            "value": 446.8697,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Micro",
            "value": 446.2053,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Small",
            "value": 474.517,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Medium",
            "value": 517.6127,
            "unit": "ms"
          },
          {
            "name": "e2e/synthetic/Deep recursion",
            "value": 463.5145,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/1K string",
            "value": 427.5813,
            "unit": "ms"
          },
          {
            "name": "e2e/strings/10K string",
            "value": 463.3272,
            "unit": "ms"
          },
          {
            "name": "e2e/args/50 args",
            "value": 447.9915,
            "unit": "ms"
          },
          {
            "name": "e2e/args/100 args",
            "value": 457.1435,
            "unit": "ms"
          },
          {
            "name": "e2e/repl/REPL session",
            "value": 463.2363,
            "unit": "ms"
          },
          {
            "name": "e2e/real/fibonacci.arl",
            "value": 813.3995,
            "unit": "ms"
          },
          {
            "name": "e2e/real/quicksort.arl",
            "value": 968.9052,
            "unit": "ms"
          },
          {
            "name": "e2e/real/graph-paths.arl",
            "value": 769.528,
            "unit": "ms"
          },
          {
            "name": "e2e/real/macro-examples.arl",
            "value": 991.4501,
            "unit": "ms"
          }
        ]
      }
    ]
  }
}