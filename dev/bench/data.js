window.BENCHMARK_DATA = {
  "lastUpdate": 1771521859869,
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
      }
    ]
  }
}