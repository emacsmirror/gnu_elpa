% -*- matlab-ts -*- %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>

% t-utils-test-indent: no-line-by-line-indent - need to improve continued properties in %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
%                                               matlab-ts-mode--i-next-line-matcher %  <{Matched rule: (matlab-ts-mode--i-block-comment-end-matcher parent 0)}>

classdef indent_class_prop_continued %  <{Matched rule: ((lambda (node parent _bol &rest _) (and node (not (string= (treesit-node-type node) "line_continuation")) (equal (treesit-node-type parent) "source_file"))) (lambda (_node _parent bol &rest _) (save-excursion (goto-char bol) (line-beginning-position))) 0)}>
    properties %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
        ListArrayHeight = struct( ... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            'Short',  {1}, ... %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>
            'Medium', {50}, ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            'Long',   {100} ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            ); %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>

        tbl = struct(... %  <{Matched rule: ((node-is "\\`\\(?:arguments_statement\\|block\\|e\\(?:num\\(?:eration\\)?\\|vents\\)\\|function_definition\\|methods\\|propert\\(?:ies\\|y\\)\\)\\'") parent 4)}>
            ... name    value %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>
            'foo',      1, ... %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>
            'bar',      1  ... %  <{Matched rule: ((parent-is "\\`arguments\\'") parent 0)}>
            ); %  <{Matched rule: ((n-p-gp "\\`\\(?:)\\|arguments\\|line_continuation\\)\\'" "\\`function_call\\'" "\\`default_value\\'") great-grand-parent 4)}>
    end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
end %  <{Matched rule: ((node-is "\\`\\(?:catch_clause\\|e\\(?:lse\\(?:\\(?:if\\)?_clause\\)\\|nd\\)\\)\\'") parent 0)}>
