This is badly documented, sorry. Just posting the code in case anyone wants to dig through it after seeing my talk

## Highlights:

### Ken Thompson hack:

```bash
git checkout ken2
git show  # <- follow these instructions
```

### Quines:

```bash
git checkout quines
diff <(./quine4.rb) quine4.rb
diff <(./quine3.rb) quine3.rb
# quine2.rb doesn't work (on purpose)

touch quine1.rb
chmod 755 quine1.rb
diff <(./quine1.rb) quine1.rb
```
