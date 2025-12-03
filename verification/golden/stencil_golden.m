
%%

function out = sc_emulator(A,w,sw)
[rows, cols] = size(A);
acc = 0;
num_windows = ceil(cols / sw);
    out = zeros(1, num_windows);

%f_res = fopen('C:\Stencil_accelerator\verification\golden\output.txt', 'w');

for i = 1:sw:cols
         acc = 0;
        win_idx = ceil(i / sw);
        for j = 1:sw
            if (i+j-1) <= cols
                acc = acc + dot(A(:, i+j-1), w(:, j));

            end
        end
   out(win_idx) = acc;
   
end

f_vec = fopen('C:/Stencil_accelerator/verification/golden/input.txt', 'w');
cnt = 0;
        for j = 1:cols
            rowstr = "";
            for i = 1:rows
                cnt = cnt+1;
                hex_val = dec2hex(typecast(single(A(i,j)), 'uint32'), 8);
                rowstr = [rowstr , hex_val];
                if(cnt == sw)
                    cnt = 0;
                    rowstr = [rowstr , newline];
                end 
            end
            fprintf(f_vec, '%s', rowstr);
            
        end
        fclose(f_vec);

        % Write output results
        f_res = fopen('C:/Stencil_accelerator/verification/golden/output.txt', 'w');
        for i = 1:num_windows
            hex_val = dec2hex(typecast(single(out(i)), 'uint32'), 8);
            fprintf(f_res, '%s\n', hex_val);
            
        end
        fclose(f_res);



end
mat = randi([0 9],3,24);
w = ones(3);

m = sc_emulator(mat,w,3);


%%

clc; clear;

% Define your range of float values
vals = 1:54;  % Example: from 1.0 to 25.0

% Preallocate string array
hex_strs = strings(1, length(vals));

for i = 1:length(vals)
    % Convert float to IEEE-754 hex
    bits = typecast(single(vals(i)), 'uint32');
    hex_strs(i) = upper(dec2hex(bits, 8));
end

% Number of hex values per row
chunk_size = 9;

% Print formatted output
for i = 1:chunk_size:length(hex_strs)
    last_idx = min(i + chunk_size - 1, length(hex_strs));
    line_str = strjoin(hex_strs(i:last_idx), '');
    disp(line_str);
end

for i = 1:9:length(vals)
    fprintf("%d %d %d %d %d %d %d %d %d",vals(i), vals(i+1),  vals(i+2), vals(i+3), vals(i+4),vals(i+5),  vals(i+6), vals(i+7), vals(i+8) );
    fprintf('\n');
end

%%
k=3;
m=9;
stencil_points = [];

for i= 0:k+1
    for j=0:k-1
        id = i+(j*m); 
        stencil_points = [stencil_points,id];

    end
end 

disp(sort(stencil_points));

%%

function C = random_real_matrix(x,y) % x is rows, y is cols 
    C = randn(x,y); 
end



function out = SODA_2d(bw,k,m,r)




end