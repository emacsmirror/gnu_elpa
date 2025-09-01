% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
function out = indent_cont_statements %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

    a = ... %  <{Matched rule: ((parent-is "\\`function_definition\\'") parent matlab-ts-mode--set-function-indent-level-for-gp)}>
        1; %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>

    b =     ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        1 + ... %  <{Matched rule: ((parent-is "\\`assignment\\'") parent 4)}>
        2 + ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
        2; %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    c = 2 * ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        1; %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>

    d = (       ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
         1 + 2); %  <{Matched rule: ((parent-is "\\`parenthesis\\'") parent 1)}>

    e = (  ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
         1 ... %  <{Matched rule: ((parent-is "\\`parenthesis\\'") parent 1)}>
        ); %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    f = 2 *  ... %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>
        (    ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
         3 + ... %  <{Matched rule: ((parent-is "\\`parenthesis\\'") parent 1)}>
         4 + (    ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
              5 * ... %  <{Matched rule: ((parent-is "\\`parenthesis\\'") parent 1)}>
              6   ... %  <{Matched rule: ((parent-is "\\`binary_operator\\'") parent 0)}>
             )    ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>
        ); ... %  <{Matched rule: ((node-is "\\`[])}]\\'") parent 0)}>

    g = 1 + 2; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

    out = a + b + c + d + e + f + g; %  <{Matched rule: ((parent-is "\\`block\\'") parent 0)}>

end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
