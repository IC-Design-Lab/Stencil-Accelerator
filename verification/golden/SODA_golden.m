function C = random_real_matrix(n,m) % x is rows, y is cols 
    C = randn(n,m); 
end

function C = random_real_matrix_3d(n,m,z) % x is rows, y is cols 
    C = randn(n,m,z); 
end

%Parameters 
unroll_factor =2;  % this is k
radius = 1;
m=16;
n=16;
p=15;
shape = "cross";
time_steps = 5;
%%
A = random_real_matrix(n,m);
W = random_real_matrix(2*radius+1,2*radius+1);
A_3d = random_real_matrix_3d(n,m,p);
%W_3d = random_real_matrix_3d(2*radius+1,2*radius+1,2*radius+1);
%A_3d = ones(n,m,p);
W_3d = ones(2*radius+1,2*radius+1,2*radius+1);
%%
function out = stencil_2d(A,W,n,m,radius,shape,unroll_factor)
center_i = radius+1 ; % starting x coord
center_j = (radius+1); % starting y coord
last_column = m-radius;
last_row = n - radius;
output = zeros(n,m);
if(shape == "cross")
    for i = center_i:last_column
        for j = center_j:last_row
            for x= -radius:radius % applying the stencil loop
                for y = -radius:radius
                    if(x == 0 || y ==0) % condition for cross shaped stencil
                    output(i,j) = output(i,j)+ A(i+x,j+y)*W(x+(radius+1),y+(radius+1));
                    end
                end
            end
        end
    end
end

if(shape == "laplace") % cross shape minus the central point
    for i = center_i:last_column
        for j = center_j:last_row
            for x= -radius:radius % applying the stencil loop
                for y = -radius:radius
                    if((x == 0 || y ==0 )&& ~(x==0) && ~(y==0)) % condition for laplace shaped stencil
                        output(i,j) = output(i,j)+ A(i+x,j+y)*W(x+(radius+1),y+(radius+1));
                    end
                end
            end
        end
    end
end

if(shape == "box")   %
    for i = center_i:last_column
        for j = center_j:last_row
            for x= -radius:radius % applying the stencil loop
                for y = -radius:radius
                    output(i,j) = output(i,j)+ A(i+x,j+y)*W(x+(radius+1),y+(radius+1)); % no condition for box shape
                end
            end
        end
    end
end

out = output;

%write ti file 
folder = sprintf('C:/Stencil_accelerator/verification/golden/test_2d_n%d_m%d_r%d_k%d_shape_%s', n,m,radius,unroll_factor, shape);
if ~exist(folder, 'dir')
    mkdir(folder);
end
input_f           = fopen(fullfile(folder,'input.txt'), 'w');
weights_f         =fopen(fullfile(folder,'weights.txt'), 'w');
output_f          =fopen(fullfile(folder,'output.txt'), 'w');
input_ieee754_f   =fopen(fullfile(folder,'input_ieee754.txt'), 'w');
weights_ieee754_f =fopen(fullfile(folder,'weights_ieee754.txt'), 'w');
output_ieee754_f  =fopen(fullfile(folder,'output_ieee754.txt'), 'w');

if(shape == "cross" || shape == "laplace") % skip first r points
cnt = 0;
for i = 1:size(A,1)
    if(i == 1)
        start_col = radius+1;
    else start_col =1;
    end
    for j = start_col:size(A,2)
        cnt = cnt+1;
        fprintf(input_f, '%f  ', A(i,j));
        fprintf(input_ieee754_f, '%08X', typecast(single(A(i,j)), 'uint32'));
        if(cnt == unroll_factor)
           fprintf(input_f, '\n');
           fprintf(input_ieee754_f, '\n');
           cnt = 0;
        end
    end
end
    for u = 1:radius % zero pad by the number of skipped values(radius)
        fprintf(input_f, '%f  ', 0.0);
        fprintf(input_ieee754_f, '%08X', typecast(single(0.0), 'uint32'));
    end
end
if(shape == "box") % no skipping condition for input stream
cnt = 0;
for i = 1:size(A,1)
    for j = 1:size(A,2)
        cnt = cnt+1;
        fprintf(input_f, '%f  ', A(i,j));
        fprintf(input_ieee754_f, '%08X', typecast(single(A(i,j)), 'uint32'));
        if(cnt == unroll_factor)
           fprintf(input_f, '\n');
           fprintf(input_ieee754_f, '\n');
           cnt = 0;
        end
    end
end
end

for i = 1:size(W,1) % weights are written as one flattened stream
    for j = 1:size(W,2)
        if(shape == "cross")
            if(i == center_i || j ==center_j) % condition for cross shaped stencil
                fprintf(weights_f, '%f  ', W(i,j));
                fprintf(weights_ieee754_f, '%08X', typecast(single(W(i,j)), 'uint32'));
            end
        elseif(shape == "box")
            fprintf(weights_f, '%f  ', W(i,j));
            fprintf(weights_ieee754_f, '%08X', typecast(single(W(i,j)), 'uint32'));
        elseif(shape =="laplace")
            if((i == center_i || j == center_j )&& ~(i==center_i) && ~(j==center_j)) % condition for laplace shaped stencil
                fprintf(weights_f, '%f  ', W(i,j));
                fprintf(weights_ieee754_f, '%08X', typecast(single(W(i,j)), 'uint32'));
            end
        end
    end
    
end

c =0;
for i = radius+1:size(output,1)-radius % outputs are examined one by one to make monitoring easier
    for j = radius+1:size(output,2)-radius
         c = c+1;
        fprintf(output_f, '%f  ', output(i,j));
        fprintf(output_ieee754_f, '%08X', typecast(single(output(i,j)), 'uint32'));
        if(c == unroll_factor)
        fprintf(output_f, '\n');
        fprintf(output_ieee754_f, '\n');
        c=0;
        end 
    end
end

fclose(input_f);
fclose(weights_f);
fclose(output_f);
fclose(input_ieee754_f);
fclose(weights_ieee754_f);
fclose(output_ieee754_f);
end 

function out = stencil_3d(A_3d,W_3d,m,n,p,radius,shape, unroll_factor,time_steps)

center_i = radius+1 ; 
center_j = (radius+1);
center_k = radius+1; 
last_column = m-radius;
last_row = n-radius;
last_depth =p-radius;
output = zeros(n,m,p);

% if(shape == "cross")
%     for t = 1:time_steps
%     for k = center_k:last_depth
%         for i = center_i:last_row
%             for j = center_j:last_column
%                 for z= -radius:radius % applying the stencil loop
%                     for y = -radius:radius
%                         for x = -radius:radius
%                             if((x == 0 && y == 0) || (x == 0 && z == 0) || (y == 0 && z == 0)) % condition for cross shaped stencil
%                                 output(i,j,k) = output(i,j,k)+ A_3d(i+x,j+y,k+z)*W_3d(x+(radius+1),y+(radius+1),z+(radius+1));
%                             end
%                         end
%                     end
%                 end
%             end
%         end
%     end
%     end
% end

if strcmp(shape,"cross")
    A_curr = A_3d;                    
    [nx, ny, nz] = size(A_curr);
    for t = 1:time_steps
        output = zeros(nx, ny, nz);
        r_t = t;     
        center_i = 1 + r_t;
        last_row = nx - r_t;
        center_j = 1 + r_t;
        last_column = ny - r_t;
        center_k = 1 + r_t;
        last_depth = nz - r_t;
        if center_i > last_row || center_j > last_column || center_k > last_depth 
            break;
        end
        for k = center_k:last_depth
            for i = center_i:last_row
                for j = center_j:last_column
                    sum_val = 0;
                    for z = -radius:radius
                        for y = -radius:radius
                            for x = -radius:radius
                                if ((x == 0 && y == 0) || (x == 0 && z == 0) || (y == 0 && z == 0))  
                                    sum_val = sum_val + A_curr(i+x,j+y,k+z) *W_3d(x+(radius+1),y+(radius+1),z+(radius+1));        
                                end
                            end
                        end
                    end
                    output(i,j,k) = sum_val;
                end
            end
        end
        A_curr = output;
    end
end

if(shape == "box") % no condition for box
    for k = center_k:last_depth
        for i = center_i:last_row
            for j = center_j:last_column
                for z= -radius:radius 
                    for y = -radius:radius
                        for x = -radius:radius
                            output(i,j,k) = output(i,j,k)+ A_3d(i+x,j+y,k+z)*W_3d(x+(radius+1),y+(radius+1),z+(radius+1));
                        end
                    end
                end
            end
        end
    end
end

if(shape == "laplace")
    for k = center_k:last_depth
        for i = center_i:last_row
            for j = center_j:last_column
                for z= -radius:radius 
                    for y = -radius:radius
                        for x = -radius:radius
                            if((x == 0 && y == 0) || (x == 0 && z == 0) || (y == 0 && z == 0) && ~(x==0) && ~(y==0) &&~(z==0)) % condition for cross shaped stencil
                                output(i,j,k) = output(i,j,k)+ A_3d(i+x,j+y,k+z)*W_3d(x+(radius+1),y+(radius+1),z+(radius+1));
                            end
                        end
                    end
                end
            end
        end
    end
end
out = output;

%write to files
folder = sprintf('C:/Stencil_accelerator/verification/golden/test_3d_n%d_m%d_p%d_r%d_k%d_shape_%s', n,m,p,radius,unroll_factor, shape);
if ~exist(folder, 'dir')
    mkdir(folder);
end
input_f           = fopen(fullfile(folder,'input.txt'), 'w');
weights_f         =fopen(fullfile(folder,'weights.txt'), 'w');
output_f          =fopen(fullfile(folder,'output.txt'), 'w');
input_ieee754_f   =fopen(fullfile(folder,'input_ieee754.txt'), 'w');
weights_ieee754_f =fopen(fullfile(folder,'weights_ieee754.txt'), 'w');
output_ieee754_f  =fopen(fullfile(folder,'output_ieee754.txt'), 'w');

if(shape == "cross" || shape == "laplace") % skip condition in 3d is different, skip the first row+radius
cnt = 0;
for k = 1:size(A_3d,3)
        if(k == 1 )
        start_row = radius+1;
        else
        start_row =1;
        end
        if(k ==p)
            ending_row = n-radius; 
        else
            ending_row = size(A_3d,1);
        end
        for i = start_row:ending_row
        if(k == 1 && i == radius+1)
            start_col = radius+1;
        else
            start_col =1;
        end
        if(k ==p && i == (n-1))
            ending_col = m-radius;
        else
            ending_col = size(A_3d,2);
        end
        for j = start_col:ending_col
        cnt = cnt+1;
        fprintf(input_f, '%f  ', A_3d(i,j,k));
        fprintf(input_ieee754_f, '%08X', typecast(single(A_3d(i,j,k)), 'uint32'));
        if(cnt == unroll_factor)
           fprintf(input_f, '\n');
           fprintf(input_ieee754_f, '\n');
           cnt = 0;
        end
        end
        end
end
end

if(shape == "box") % no skipping condition for input stream
cnt = 0;
for k = 1:size(A_3d,3)
    for i = 1:size(A_3d,1)
        for j =1:size(A_3d,2)
        cnt = cnt+1;
        fprintf(input_f, '%f  ', A_3d(i,j,k));
        fprintf(input_ieee754_f, '%08X', typecast(single(A_3d(i,j,k)), 'uint32'));
        if(cnt == unroll_factor)
           fprintf(input_f, '\n');
           fprintf(input_ieee754_f, '\n');
        cnt = 0;
        end
        end
    end
end
end

for k = 1:size(W_3d,3)
    for i = 1:size(W_3d,1)
        for j = 1:size(W_3d,2)
            if(shape == "cross")
            c = radius + 1;
            dist = abs(i-c) + abs(j-c) + abs(k-c);
            if dist <= radius
                fprintf(weights_f, '%f  ', W_3d(i,j,k));
                fprintf(weights_ieee754_f, '%08X', typecast(single(W_3d(i,j,k)), 'uint32'));
            end
            end
        end
    end
end

shrink = time_steps;
count = 0;
starting_depth = 1 + shrink;
ending_depth   = p - shrink;
for k = starting_depth:ending_depth 
    starting_row = 1 + shrink;
    ending_row   = n - shrink;
    for i = starting_row:ending_row
         starting_col = 1 + shrink;
        ending_col   = m - shrink;
        for j = starting_col:ending_col
            count = count+1;
        fprintf(output_f, '%f  ', output(i,j,k));
        fprintf(output_ieee754_f, '%08X', typecast(single(output(i,j,k)), 'uint32'));
        if(count == unroll_factor)
        fprintf(output_f, '\n');
        fprintf(output_ieee754_f, '\n');
        count=0;
        end
        end
    end
end

fclose(input_f);
fclose(weights_f);
fclose(output_f);
fclose(input_ieee754_f);
fclose(weights_ieee754_f);
fclose(output_ieee754_f);

end

%s= stencil_2d(A,W,m,n,radius,shape,unroll_factor);
d= stencil_3d(A_3d,W_3d,m,n,p,radius,shape,unroll_factor,time_steps);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

