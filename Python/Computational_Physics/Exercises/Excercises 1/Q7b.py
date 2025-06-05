n = int(input("N: "))

temp = tuple()
end = tuple()
for i in range(n,0,-1):
    temp += i,
    end += temp,
print(end)

print(list(end))

#x = [i for i in range(n,0,-1)]
