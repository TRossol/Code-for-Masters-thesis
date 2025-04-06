% Define expectations as seperate symbolic variables
syms E_D E_D2 E_D3 E_D4 E_Y E_Y2 E_Y3 E_Y4 E_Z E_Z2 E_Z3 E_Z4 real; 
syms E_DY E_D2Y2 E_D3Y E_DZ E_DZ2 E_D2Z E_D2Z2 E_D2Z3 E_D2Z4 E_D3Z E_D3Z2 E_D3Z3 E_D4Z E_D4Z2 E_YZ E_Y2Z2 real;
syms E_DY2Z E_DYZ2 E_D2YZ E_D2YZ2 E_D3YZ E_DYZ3 real;

% Matrix with symbolic expectations
A = [E_D4 - E_D2^2, E_D3Z - E_D2 * E_DZ, E_D3Y - E_D2 * E_DY, E_D2YZ - E_D2 * E_YZ, E_D3Z2 - E_D2 * E_DZ2, E_D4Z - E_D2 * E_D2Z;
     E_D3Z - E_D2 * E_DZ, E_D2Z2 - E_DZ^2, E_D2YZ - E_DZ * E_DY, E_DYZ2 - E_DZ * E_YZ, E_D2Z3 - E_DZ * E_DZ2, E_D3Z2 - E_DZ * E_D2Z;
     E_D3Y - E_D2 * E_DY, E_D2YZ - E_DZ * E_DY, E_D2Y2 - E_DY^2, E_DY2Z - E_DY * E_YZ, E_D2YZ2 - E_DY * E_DZ2, E_D3YZ - E_DY * E_D2Z;
     E_D2YZ - E_D2 * E_YZ, E_DYZ2 - E_DZ * E_YZ, E_DY2Z - E_DY * E_YZ, E_Y2Z2 - E_YZ^2, E_DYZ3 - E_DZ2 * E_YZ, E_D2YZ2 - E_YZ * E_D2Z;
     E_D3Z2 - E_D2 * E_DZ2, E_D2Z3 - E_DZ * E_DZ2, E_D2YZ2 - E_DY * E_DZ2, E_DYZ3 - E_YZ * E_DZ2, E_D2Z4 - E_DZ2^2, E_D3Z3 - E_DZ2 * E_D2Z;
     E_D4Z - E_D2 * E_D2Z, E_D3Z2 - E_DZ * E_D2Z, E_D3YZ - E_DY * E_D2Z, E_D2YZ2 - E_YZ * E_D2Z, E_D3Z3 - E_DZ2 * E_D2Z, E_D4Z2 - E_D2Z^2];

% Vector with symbolic expectations
v = [(E_DZ2 * (E_YZ * E_D2Z - E_DY * E_DZ2)) / (E_D2 * E_DZ2 - E_DZ * E_D2Z)^2; 
    (E_D2Z * (E_DY * E_DZ2 - E_YZ * E_D2Z)) / (E_D2 * E_DZ2 - E_DZ * E_D2Z)^2; 
    E_DZ2 / (E_D2 * E_DZ2 - E_DZ * E_D2Z); 
    (- E_D2Z) / (E_D2 * E_DZ2 - E_DZ * E_D2Z); 
    (E_D2Z * (E_D2 * E_YZ - E_DZ * E_DY)) / (E_D2 * E_DZ2 - E_DZ * E_D2Z)^2; 
    (E_DZ2 * (E_DZ * E_DY - E_D2 * E_YZ)) / (E_D2 * E_DZ2 - E_DZ * E_D2Z)^2];

% Matrix vector multiplication
B = A * v;

% Symplification of the result
simplified_B = simplify(B);

% Calculation of scalar product
scalar_product = dot(v, simplified_B);
simplified_scalar_product = simplify(scalar_product);
disp(simplified_scalar_product);
