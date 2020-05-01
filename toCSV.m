files = dir("../behaviour");
nFile = length(files);
mkdir("../data");
for i = 4 : nFile
    file = sprintf('../behaviour/%s', files(i).name);
    load(file);
    junk = cell2table(squeeze(struct2cell(data))', 'VariableNames', fieldnames(data));
    writetable(junk, sprintf('%s.csv', files(i).name(1 : end -4)));
end